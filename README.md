# open-flir

Open FLIR is the first open source, medium resolution, and broadband forward looking infrared (FLIR) camera.

![The Open FLIR device](/docs/images/demo.jpg?raw=true)

This thermal camera is based on a Heimann Sensor thermopile array module, detailed in ```docs/HeimannArrays9-9-16.pdf``` and the sample datasheet (```docs/HTPA 32x32d L2.1_0.8 (Hi)S Rev2 Datasheet.pdf```.) Previous attempts at open source thermal cameras have been based on the Melexis MLX90621, an earlier and lower resolution thermopile array. For comparison, the datasheet for this part is also available at ```docs/MLX90621-Datasheet-Melexis.pdf```.

Code is in ```source/GUI/```, and should work out of the box on a Raspberry Pi with I2C enabled. It is dependent on OpenCV, and a handful of other Python packages.

Models for 3D printing are in ```case/```.

Research on noise characteristics of the camera and possible uses/limitations for gas sensing are archived in ```noise/```.

## Hardware

The hardware involved is extremely straightforward: a Raspberry Pi communicates with the Heimann sensor over I2C and displays the images to the user. The sensor should be connected to the Raspberry Pi GPIO pins 3 and 5, and can be powered from the 3.3V and ground pins of the GPIO port. This does not interfere with the use of SPI for touch screen communication. The system is powered from a USB power bank.

### Bill of materials

* Raspberry Pi Zero (or Raspberry Pi)
* LCD screen ([e.g.](https://www.amazon.com/SunFounder-Monitor-Display-800X480-Raspberry/dp/B01HXSFIH6))
** HDMI cable
* Raspberry Pi camera ([e.g.](https://www.amazon.com/dp/B01LY05LOE/ref=sr_ph_1?ie=UTF8&qid=1485905985&sr=sr-1&keywords=raspberry+pi+camera+zero))
* Heimann HTPA32x32dL3.6/0.9 (purchased through [Boston Electronics](http://www.boselec.com/))
* USB power bank ([e.g.](https://www.amazon.com/Anker-bar-Sized-Portable-High-Speed-Technology/dp/B00P7N0320/ref=sr_1_5?ie=UTF8&qid=1485906277&sr=8-5&keywords=anker+power+bank))
** USB cable with switch
* Case (3D printed files included in repository)
* Bracket for attaching Heimann sensor to case

## Software

The application consists of a Python class for interfacing with the module (```htpa.py```) and a GUI (```dualcam.py```). The GUI allows for control of the sensor clock frequency, current, and bias. The default settings are the settings used for the module factory calibration, and seem to produce the best results.

## Known issues

* The sensor calibration programmed into the EEPROM at the factory does not seem to match the noise profile of the images captured from the device.
* The HDMI cable is super janky.

## Physical assembly

![All parts](/docs/images/DSC01492.JPG?raw=true)

![IR sensor assembly](/docs/images/DSC01496.JPG?raw=true)

![IR sensor in filter holder](/docs/images/DSC01499.JPG?raw=true)

![IR sensor placed in case](/docs/images/DSC01504.JPG?raw=true)

![With bracket mounted](/docs/images/DSC01506.JPG?raw=true)

![Stress relief](/docs/images/DSC01509.JPG?raw=true)

![Visible camera mounted](/docs/images/DSC01514.JPG?raw=true)

![Screen and front case](/docs/images/DSC01517.JPG?raw=true)

![Screen mounted in front case](/docs/images/DSC01521.JPG?raw=true)

![Battery mounted underneath screen](/docs/images/DSC01523.JPG?raw=true)

![Switch glued to top of case](/docs/images/DSC01525.JPG?raw=true)

![HDMI cable connected](/docs/images/DSC01531-alt.jpg?raw=true)

![HDMI and USB cable connected](/docs/images/DSC01532-alt.jpg?raw=true)

![Back of case secured](/docs/images/DSC01536.JPG?raw=true)

![Booting up (with proper display settings, the whole screen is used.)](/docs/images/DSC01538.JPG?raw=true)
