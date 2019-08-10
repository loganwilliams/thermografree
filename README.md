# Thermografree

Thermografree is the first open source, medium resolution, and broadband forward looking infrared (FLIR) camera. It can be used as a thermographic camera, or for scientific imaging applications in the 2-18 um range.

![The Thermografree device](/docs/images/demo.jpg?raw=true)

This thermal camera is based on a Heimann Sensor thermopile array module, detailed in `docs/HeimannArrays9-9-16.pdf` and the sample datasheet (`docs/HTPA 32x32d L2.1_0.8 (Hi)S Rev2 Datasheet.pdf`). Previous attempts at open source thermal cameras have been based on the Melexis MLX90621, an earlier and lower resolution thermopile array. For comparison, the datasheet for this part is also available at `docs/MLX90621-Datasheet-Melexis.pdf`.

Code is in ```src/```, and should work out of the box on a Raspberry Pi with I2C enabled. If you are using an older (2015-2017) verison of the device, you will have to change the HTPA initialization to add an explicit `revision="2017"` in `dualcam.py`. Installation instructions are below.

OpenSCAD models for 3D printing are in ```case/```. The case and hardware (including the screen, cabling, and battery) are very minimum viable prototype, and should not in anyway be taken as "best practices" hardware advice.

Research on noise characteristics of the camera and possible uses/limitations for gas sensing are archived in ```docs/noise-analysis/```.

## Hardware

The hardware involved is extremely straightforward: a Raspberry Pi communicates with the Heimann sensor over I2C and displays the images to the user. The sensor should be connected to the Raspberry Pi GPIO pins 3 and 5, and can be powered from the 3.3V and ground pins of the GPIO port. This does not interfere with the use of SPI for touch screen communication. The system is powered from a USB power bank.

### Bill of materials

* Raspberry Pi Zero (or Raspberry Pi)
* LCD screen ([e.g.](https://www.amazon.com/SunFounder-Monitor-Display-800X480-Raspberry/dp/B01HXSFIH6)) and HDMI cable
* Raspberry Pi camera ([e.g.](https://www.amazon.com/dp/B01LY05LOE/ref=sr_ph_1?ie=UTF8&qid=1485905985&sr=sr-1&keywords=raspberry+pi+camera+zero))
* Heimann HTPA32x32dL3.6/0.9 (purchased through [Boston Electronics](http://www.boselec.com/))
* USB power bank ([e.g.](https://www.amazon.com/Anker-bar-Sized-Portable-High-Speed-Technology/dp/B00P7N0320/ref=sr_1_5?ie=UTF8&qid=1485906277&sr=8-5&keywords=anker+power+bank)) and USB cable with switch
* Case (3D printed files included in repository)
* Bracket for attaching Heimann sensor to case

## Software

The application consists of a Python class for interfacing with the module (```htpa.py```) and a GUI (```dualcam.py```). The GUI allows for control of the sensor clock frequency, current, and bias. The default settings are the settings used for the module factory calibration, and seem to produce the best results.

### Installation

#### Installing pre-requisites

The Python software has several pre-requisites. To install them on your Raspberry Pi, run the following commands while you have an internet connection.

```
sudo apt-get install python-pip ipython
sudo apt-get install python-numpy python-scipy
sudo apt-get install libopencv-dev python-opencv
sudo pip install python-periphery picamera imutils pillow
```

#### Enable I2C

Follow [Adafruit's tutorial](https://learn.adafruit.com/adafruits-raspberry-pi-lesson-4-gpio-setup/configuring-i2c).

#### Enabling I2C repeated starts

The I2C hardware on the Raspberry Pi needs to be configured to support "repeated starts." To do this, add the following line to ```/etc/modprobe.d/i2c.conf```:

```
options i2c-bcm2708 combined=1
```

If you are not using a Raspberry Pi, enabling repeated starts will likely require a different configuration. For more information, see [the blog post I wrote on this topic](http://exclav.es/2016/10/26/talkin-ir/).

## Known issues

* The sensor calibration programmed into the EEPROM at the factory does not seem to match the noise profile of the images captured from the device.
* The HDMI cable connection to a Raspberry Pi Zero is a hack. If you use the screen documented here, we suggest a Raspberry Pi 3.

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
