from Tkinter import *
import cv2
import numpy as np
from PIL import ImageTk, Image
import Queue
import time
import threading
from picamera.array import PiRGBArray
from picamera import PiCamera
from imutils.video import VideoStream
import imutils
from htpa import *
import pickle

def ir_capture(image_queue, calibrate, stop, commands):
    dev = HTPA(0x1A)
    max_val = 0
    min_val = 0

    while not stop.isSet():

        while not commands.empty():
            cmd = commands.get()
            print "applying command"
            print cmd

            if cmd[0] == 'bias':
                dev.set_bias_current(cmd[1])
            elif cmd[0] == 'clock':
                dev.set_clock_speed(cmd[1])
            elif cmd[0] == 'cm':
                dev.set_cm_current(cmd[1])

        if calibrate.isSet():
            print "IR sensor calibrating"
            dev.measure_observed_offset()
            max_val = 0
            min_val = 0
            calibrate.clear()

        print("Capturing image")
        (pixel_values, ptats) = dev.capture_image()
        im = dev.temperature_compensation(pixel_values, ptats)
        im = dev.offset_compensation(im)
        im = dev.sensitivity_compensation(im)

        if (max_val == min_val):
            max_val = np.max(im)
            min_val = np.min(im)
        else:
            max_val = max_val * 0.75 + np.max(im) * 0.25
            min_val = min_val * 0.75 + np.min(im) * 0.25

        im = (im - min_val) / (max_val - min_val)

    	im[im > 1] = 1
    	im[im < 0] = 0
    	im = np.fliplr(im)

        new_image = np.uint8(255 * im)
        image_queue.put(new_image)

    print "Closing, cleaning up"
    dev.close()

# user interface component

class DualCamera():
    def __init__(self):
        self.root = Tk()
        self.root.wm_title("Dual Cam")

        self.saving = False
        self.frames = []
        self.overlay = False

        self.image_left = ImageTk.PhotoImage(image=Image.fromarray(np.uint8(np.zeros((256, 256)))))
        self.image_panel_left = Label(self.root, image = self.image_left)
        self.image_panel_left.grid(row = 0, column = 0, columnspan=2)

        self.image_right = ImageTk.PhotoImage(image=Image.fromarray(np.uint8(256 * np.random.rand(256, 256))))
        self.image_panel_right = Label(self.root, image = self.image_right)
        self.image_panel_right.grid(row = 0, column = 2, columnspan=2)

        self.save_button = Button(width = 10, height = 2, text = 'Save', command=self.save)
        self.save_button.grid(row = 1, column = 0)

        self.calibrate_button = Button(width = 10, height = 2, text = 'Calibrate', command=self.calibrate)
        self.calibrate_button.grid(row = 1, column = 1)

        self.close_button = Button(width = 10, height = 2, text = 'Close', command=self.quit)
        self.close_button.grid(row = 1, column = 3)

        self.overlay_button = Button(width=10, height=2, text='Overlay', command=self.toggle_overlay)
        self.overlay_button.grid(row = 1, column = 2)

        self.bias_slider = Scale(self.root, from_=0, to=31, length=400, orient=HORIZONTAL, command=self.bias)
        self.bias_slider.grid(row = 2, column = 1, columnspan=3)
    	self.bias_label = Label(self.root, text="Bias current")
    	self.bias_label.grid(row=2, column=0)

        self.clock_slider = Scale(self.root, from_=0, to=63, length=400, orient=HORIZONTAL, command=self.clock)
        self.clock_slider.grid(row = 3, column = 1, columnspan=3)
    	self.clock_label = Label(self.root, text="Clock speed")
    	self.clock_label.grid(row=3, column=0)

        self.cm_slider = Scale(self.root, from_=0, to=31, length=400, orient=HORIZONTAL, command=self.cm)
        self.cm_slider.grid(row = 4, column = 1, columnspan=3)
    	self.cm_label = Label(self.root, text="CM current")
    	self.cm_label.grid(row=4, column=0)

        # set default positions
    	self.cm_slider.set(0x0C)
    	self.clock_slider.set(0x15)
    	self.bias_slider.set(0x05)


        # initialize visible camera
        self.vs = VideoStream(usePiCamera=True).start()

        # thread for reading from sensor hardware intro an image queue           
        self.ir_images = Queue.LifoQueue()
        self.ir_commands = Queue.Queue()
        self.ir_calibrate = threading.Event()
        self.ir_stop = threading.Event()

        self.capture_thread = threading.Thread(
                        target=ir_capture,
                        name="capture_thread",
                        args=[self.ir_images, self.ir_calibrate, self.ir_stop, self.ir_commands]
                        )

        self.capture_thread.start()

        self.ticktock()
        self.root.mainloop()

    def ticktock(self):
        # grab an image from the camera
        frame = self.vs.read()
        changed = False

        if frame is not None:
            frame = imutils.resize(frame, height=240)
            image = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
            self.last_vis_frame = image
            image = Image.fromarray(image)

            if not self.overlay:
                self.image_right = ImageTk.PhotoImage(image=image)
                self.image_panel_right.configure(image=self.image_right);

            changed = True

        if not self.ir_images.empty():
            ir_frame = self.ir_images.get()
            with self.ir_images.mutex:
                self.ir_images.queue = []
            ir_image = imutils.resize(ir_frame, height=240, inter=cv2.INTER_LINEAR)
            self.last_ir_frame = ir_image
            self.image_left = ImageTk.PhotoImage(image=Image.fromarray(ir_image))
            self.image_panel_left.configure(image=self.image_left)
            changed = True
        else:
            ir_image = None

        if changed and self.overlay:
            overlay_image = np.zeros_like(self.last_vis_frame)

            overlay_image[:,:,0] = 0.125 * self.last_vis_frame[:,:,0] + 0.25 * self.last_vis_frame[:,:,1] + 0.125 * self.last_vis_frame[:,:,2]
            overlay_image[:,:,1] = 0.125 * self.last_vis_frame[:,:,0] + 0.25 * self.last_vis_frame[:,:,1] + 0.125 * self.last_vis_frame[:,:,2]
            overlay_image[:,:,2] = 0.125 * self.last_vis_frame[:,:,0] + 0.25 * self.last_vis_frame[:,:,1] + 0.125 * self.last_vis_frame[:,:,2]

            mapped_image = cv2.applyColorMap(self.last_ir_frame, cv2.COLORMAP_HOT)

            overlay_image[:,40:280,0] += 0.5 * mapped_image[:,:,0]
            overlay_image[:,40:280,1] += 0.5 * mapped_image[:,:,1]
            overlay_image[:,40:280,2] += 0.5 * mapped_image[:,:,2]
            
            self.image_right = ImageTk.PhotoImage(image=Image.fromarray(overlay_image))
            self.image_panel_right.configure(image=self.image_right)

        if self.saving:
            self.frames.append((frame, ir_frame))

        if not self.ir_calibrate.isSet():
            self.calibrate_button.configure(text = 'Calibrate', command=self.calibrate, state="normal")

        self.root.after(100, self.ticktock)

    def quit(self):
        self.vs.stop()
        self.ir_stop.set()
        self.root.quit()

    def save(self):
        self.save_button.configure(text = 'Stop Saving', command=self.stop_save)
        self.saving = True
        self.frames = []

    def stop_save(self):
        self.save_button.configure(text = 'Save', command=self.save)
        now = time.strftime("%Y-%m-%dT%H:%M:%S")
        pickle.dump(self.frames, open(now + ".p", "wb"))
        self.frames = []

    def calibrate(self):
        self.calibrate_button.configure(text = 'Calibrating...', state="disabled")
        self.ir_calibrate.set()

    def cm(self, val):
        val = int(val)
        self.ir_commands.put(('cm', val))

    def bias(self, val):
        val = int(val)
        self.ir_commands.put(('bias', val))

    def clock(self, val):
        val = int(val)
        self.ir_commands.put(('clock', val))

    def toggle_overlay(self):
        if self.overlay:
            self.overlay = False
            self.overlay_button.configure(text = 'Overlay')
        else:
            self.overlay = True
            self.overlay_button.configure(text = 'No Overlay')

app = DualCamera()
