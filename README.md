# open-flir

This repository contains code and hardware design files for an open source forward looking infrared (FLIR) thermal camera.

The thermal camera is based on a Heimann Sensor module, detailed in ```docs/HeimannArrays9-9-16.pdf``` and the sample datasheet (```docs/HTPA 32x32d L2.1_0.8 (Hi)S Rev2 Datasheet.pdf```.) Previous attempts at open source thermal cameras have been based on the Melexis MLX90621. The datasheet for this part is also available at ```docs/MLX90621-Datasheet-Melexis.pdf```.

Code is in ```source/GUI/```, and should work out of the box on a Raspberry Pi with I2C enabled. It is dependent on OpenCV, and a handful of other Python packages.

Models for 3D printing are in ```case/```.

Research on noise characteristics of the camera and possible uses/limitations for gas sensing are archived in ```noise/```.

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
