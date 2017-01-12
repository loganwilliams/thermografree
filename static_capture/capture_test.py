from periphery import I2C
import time
import numpy as np
import copy
import pickle

i2c = I2C("/dev/i2c-1")

device_address = 0x1A

def generate_command(register, value):
	return [I2C.Message([register, value])]

def send_command(cmd, wait=True):
	i2c.transfer(device_address, cmd)
	if wait:
		time.sleep(0.005) # sleep for 5 ms

wakeup_and_blind = generate_command(0x01, 0x01) # wake up the device
adc_res = generate_command(0x03, 0x0C) # set ADC resolution to 16 bits
bias_top = generate_command(0x04, 0x05) # 
bias_bottom = generate_command(0x05, 0x05) #
clk_speed = generate_command(0x06, 0x15)
cm_top = generate_command(0x07, 0x0C) # BPA
cm_bottom = generate_command(0x08, 0x0C) # BPA
pull_ups = generate_command(0x09, 0x88)
sleep = generate_command(0x01, 0x00)

def generate_expose_block_command(block):
	return generate_command(0x01, 0x09 + (block << 4))

print("Sending commands")

send_command(wakeup_and_blind)
send_command(adc_res)
send_command(bias_top)
send_command(bias_bottom)
send_command(clk_speed)
send_command(cm_top)
send_command(cm_bottom)
send_command(pull_ups)

frames = []

for i in range(20):
	print("Capturing image " + str(i))

	pixel_values = np.zeros(1024)
	ptats = np.zeros(8)

	for block in range(4):
		print("Exposing block " + str(block))
		send_command(generate_expose_block_command(block), wait=False)

		query = [I2C.Message([0x02]), I2C.Message([0x00], read=True)]
		expected = 1 + (block << 2)

		done = False

		while not done:
			i2c.transfer(device_address, query)

			if not (query[1].data[0] == expected):
				print("Not ready, received " + str(query[1].data[0]) + ", expected " + str(expected))
				time.sleep(0.005)
			else:
				done = True

		read_block = [I2C.Message([0x0A]), I2C.Message([0x00]*258, read=True)]
		i2c.transfer(device_address, read_block)
		top_data = np.array(copy.copy(read_block[1].data))

		read_block = [I2C.Message([0x0B]), I2C.Message([0x00]*258, read=True)]
		i2c.transfer(device_address, read_block)
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

	frames.append((pixel_values, ptats))

	time.sleep(1)

send_command(sleep)

pickle.dump(frames, open("capture.p", "wb"))
