from periphery import I2C
import time
import numpy as np
import copy
import struct

class HTPA:
	def __init__(self, address):
		self.address = address
		self.i2c = I2C("/dev/i2c-1")

		wakeup_and_blind = self.generate_command(0x01, 0x01) # wake up the device
		adc_res = self.generate_command(0x03, 0x0C) # set ADC resolution to 16 bits
		pull_ups = self.generate_command(0x09, 0x88)

		print("Initializing capture settings")

		self.send_command(wakeup_and_blind)
		self.send_command(adc_res)
		self.send_command(pull_ups)

		self.set_bias_current(0x05)
		self.set_clock_speed(0x15)
		self.set_cm_current(0x0C)

		print ("Grabbing EEPROM data")

		eeprom = self.get_eeprom()
		self.extract_eeprom_parameters(eeprom)
		self.eeprom = eeprom

		# initialize offset to zero
		self.offset = np.zeros((32, 32))

	def set_bias_current(self, bias):
		if bias > 31:
			bias = 31
		if bias < 0:
			bias = 0

		bias = int(bias)

		bias_top = self.generate_command(0x04, bias)
		bias_bottom = self.generate_command(0x05, bias)

		self.send_command(bias_top)
		self.send_command(bias_bottom)

	def set_clock_speed(self, clk):
		if clk > 63:
			clk = 63
		if clk < 0:
			clk = 0

		clk = int(clk)

		clk_speed = self.generate_command(0x06, clk)

		self.send_command(clk_speed)

	def set_cm_current(self, cm):
		if cm > 31:
			cm = 31
		if cm < 0:
			cm = 0

		cm = int(cm)

		cm_top = self.generate_command(0x07, cm)
		cm_bottom = self.generate_command(0x08, cm)

		self.send_command(cm_top)
		self.send_command(cm_bottom)

	def get_eeprom(self, eeprom_address=0x50):
		# My Raspberry pi keeps timing-out here, so we split it into
		# two different transfers:...
		q1 = [I2C.Message([0x00, 0x00]), I2C.Message([0x00]*4000, read=True)]
		q2 = [I2C.Message([0x0f, 0xa0]), I2C.Message([0x00]*4000, read=True)]
		self.i2c.transfer(eeprom_address, q1)
		self.i2c.transfer(eeprom_address, q2)
		return np.array(q1[1].data + q2[1].data)

	def extract_eeprom_parameters(self, eeprom):
		self.VddComp = eeprom[0x0540:0x0740:2] + (eeprom[0x0541:0x0740:2] << 8)

		ThGrad =   eeprom[0x0740:0x0F40:2] + (eeprom[0x0741:0x0F40:2] << 8)
		ThGrad = [tg - 65536 if tg >= 32768 else tg for tg in ThGrad]
		ThGrad = np.reshape(ThGrad, (32, 32))
		ThGrad[16:,:] = np.flipud(ThGrad[16:,:])
		self.ThGrad = ThGrad

		ThOffset = eeprom[0x0F40:0x1740:2] + (eeprom[0x0F41:0x1740:2] << 8)
		ThOffset = np.reshape(ThOffset, (32, 32))
		ThOffset[16:,:] = np.flipud(ThOffset[16:,:])
		self.ThOffset = ThOffset

		P =        eeprom[0x1740::2] + (eeprom[0x1741::2] << 8)
		P = np.reshape(P, (32, 32))
		P[16:, :] = np.flipud(P[16:,:])
		self.P = P

		epsilon = float(eeprom[0x000D])
		GlobalGain = eeprom[0x0055] + (eeprom[0x0056] << 8)
		Pmin = eeprom[0x0000:0x0004]
		Pmax = eeprom[0x0004:0x0008]
		Pmin = struct.unpack('f', reduce(lambda a,b: a+b, [chr(p) for p in Pmin]))[0]
		Pmax = struct.unpack('f', reduce(lambda a,b: a+b, [chr(p) for p in Pmax]))[0]
		self.PixC = (P * (Pmax - Pmin) / 65535. + Pmin) * (epsilon / 100) * float(GlobalGain) / 100

		self.gradScale = eeprom[0x0008]
		self.VddCalib = eeprom[0x0046] + (eeprom[0x0047] << 8)
		self.Vdd = 3280.0
		self.VddScaling = eeprom[0x004E]

		PTATgradient = eeprom[0x0034:0x0038]
		self.PTATgradient = struct.unpack('f', reduce(lambda a,b: a+b, [chr(p) for p in PTATgradient]))[0]
		PTAToffset = eeprom[0x0038:0x003c]
		self.PTAToffset = struct.unpack('f', reduce(lambda a,b: a+b, [chr(p) for p in PTAToffset]))[0]

	def temperature_compensation(self, im, ptat):
	    comp = np.zeros((32,32))
	    
	    Ta = np.mean(ptat) * self.PTATgradient + self.PTAToffset
		#     temperature compensated voltage
	    comp = ((self.ThGrad * Ta) / pow(2, self.gradScale)) + self.ThOffset

	    Vcomp = np.reshape(im,(32, 32)) - comp
	    return Vcomp

	def offset_compensation(self, im):
		return im - self.offset

	def sensitivity_compensation(self, im):
		return im/self.PixC

	def measure_observed_offset(self):
		print("Measuring observed offsets")
		print("    Camera should be against uniform temperature surface")
		mean_offset = np.zeros((32, 32))

		for i in range(10):
			print("    frame " + str(i))
			(p, pt) = self.capture_image()
			im = self.temperature_compensation(p, pt)
			mean_offset += im/10.0

		self.offset = mean_offset

	def measure_electrical_offset(self):
		(offsets, ptats) = self.capture_image(blind=True)
		self.offset = self.temperature_compensation(offsets, ptats)

	def capture_image(self, blind=False):
		pixel_values = np.zeros(1024)
		ptats = np.zeros(8)

		for block in range(4):
			# print("Exposing block " + str(block))
			self.send_command(self.generate_expose_block_command(block, blind=blind), wait=False)

			query = [I2C.Message([0x02]), I2C.Message([0x00], read=True)]
			expected = 1 + (block << 4)

			done = False

			while not done:
				self.i2c.transfer(self.address, query)

				if not (query[1].data[0] == expected):
					# print("Not ready, received " + str(query[1].data[0]) + ", expected " + str(expected))
					time.sleep(0.005)
				else:
					done = True

			read_block = [I2C.Message([0x0A]), I2C.Message([0x00]*258, read=True)]
			self.i2c.transfer(self.address, read_block)
			top_data = np.array(copy.copy(read_block[1].data))

			read_block = [I2C.Message([0x0B]), I2C.Message([0x00]*258, read=True)]
			self.i2c.transfer(self.address, read_block)
			bottom_data = np.array(copy.copy(read_block[1].data))

			top_data = top_data[1::2] + (top_data[0::2] << 8)
			bottom_data = bottom_data[1::2] + (bottom_data[0::2] << 8)

			pixel_values[(0+block*128):(128+block*128)] = top_data[1:]
			# bottom data is in a weird shape
			pixel_values[(992-block*128):(1024-block*128)] = bottom_data[1:33]
			pixel_values[(960-block*128):(992-block*128)] = bottom_data[33:65]
			pixel_values[(928-block*128):(960-block*128)] = bottom_data[65:97]
			pixel_values[(896-block*128):(928-block*128)] = bottom_data[97:]

			ptats[block] = top_data[0]
			ptats[7-block] = bottom_data[0]

		pixel_values = np.reshape(pixel_values, (32, 32))

		return (pixel_values, ptats)

	def generate_command(self, register, value):
		return [I2C.Message([register, value])]

	def generate_expose_block_command(self, block, blind=False):
		if blind:
			return self.generate_command(0x01, 0x09 + (block << 4) + 0x02)
		else:
			return self.generate_command(0x01, 0x09 + (block << 4))

	def send_command(self, cmd, wait=True):
		self.i2c.transfer(self.address, cmd)
		if wait:
			time.sleep(0.005) # sleep for 5 ms

	def close(self):
		sleep = self.generate_command(0x01, 0x00)
		self.send_command(sleep)
