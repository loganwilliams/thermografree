from periphery import I2C
import time
import numpy as np
import copy
import pickle

i2c = I2C("/dev/i2c-1")

device_address = 0x50

query = [I2C.Message([0x00, 0x00]), I2C.Message([0x00]*8000, read=True)]

i2c.transfer(device_address, query)

pickle.dump(query[1].data, open("eeprom.p", "wb"))

print(query[1].data)

