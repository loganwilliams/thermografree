from periphery import I2C
import time
import numpy as np
import copy
import pickle

i2c = I2C("/dev/i2c-1")

device_address = 0x1A

def generate_command(register, value):
	return [I2C.Message([register, value])]

def send_command(cmd)
	i2c.transfer(device_addess, cmd)
	time.sleep(0.005) # sleep for 5 ms

wakeup = generate_command(0x01, 0x01) # wake up the device
adc_res = generate_command(0x03, 0x0C) # set ADC resolution to 16 bits
bias_top = generate_command(0x04, 0x0C) # 
bias_bottom = generate_command(0x05, 0x0C) #
clk_speed = generate_command(0x06, 0x14)
cm_top = generate_command(0x07, 0x0C)
cm_bottom = generate_command(0x08, 0x0C)
pull_ups = generate_command(0x09, 0x88)

def generate_expose_block_command(block):
	return generate_command(0x01, 0x09 + (block << 4))

print("Sending commands")

send_command(wakeup)
send_command(adc_res)
send_command(bias_top)
send_command(bias_bottom)
send_command(clk_speed)
send_command(cm_top)
send_command(cm_bottom)
send_command(pull_ups)

pixel_values = np.zeros(1024)
ptats = np.zeros(8)

for block in range(4):
	print("Exposing block " + str(block))
	send_command(generate_expose_block_command(block))

	query = [I2C.Message([0x02]), I2C.Message([0x00], read=True)]
	expected = 1 + (block << 2)

	done = False

	while not done:
		i2c.transfer(device_addess, query)

		if not (query[1].data == expected):
			print("Not ready, received " + str(query[1].data) + ", expected " + str(expected))
			time.sleep(0.03)
		else:
			done = True

	top_read_block = [I2C.Message([0x0A]), I2C.Message([0x00]*258, read=True)]
	i2c.transfer(device_address, top_read_block)
	top_data = read_block[1].data[1::2] + (read_block[1].data[0::2] << 8)

	bottom_read_block = [I2C.Message([0x0B]), I2C.Message([0x00]*258, read=True)]
	i2c.transfer(device_address, bottom_read_block)
	bottom_data = read_block[1].data[1::2] + (read_block[1].data[0::2] << 8)

	pixel_values[(0+block*128):(127+block*128)] = copy.copy(top_data[1:])
	pixel_values[(896-block*128):(1024-block*128)] = copy.copy(np.flipud(bottom_data[1:]))
	ptats[block] = top_data[0]
	ptats[7-block] = bottom_data[0]

pickle.dump((pixel_values, ptats), open("capture.p", "wb"))
