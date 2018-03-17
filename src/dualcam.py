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
import copy

colormap = np.array([[[0,0,4],[1,0,5],[1,1,6],[1,1,8],[2,1,10],[2,2,12],[2,2,14],[3,2,16],[4,3,18],[4,3,20],[5,4,23],[6,4,25],[7,5,27],[8,5,29],[9,6,31],[10,7,34],[11,7,36],[12,8,38],[13,8,41],[14,9,43],[16,9,45],[17,10,48],[18,10,50],[20,11,52],[21,11,55],[22,11,57],[24,12,60],[25,12,62],[27,12,65],[28,12,67],[30,12,69],[31,12,72],[33,12,74],[35,12,76],[36,12,79],[38,12,81],[40,11,83],[41,11,85],[43,11,87],[45,11,89],[47,10,91],[49,10,92],[50,10,94],[52,10,95],[54,9,97],[56,9,98],[57,9,99],[59,9,100],[61,9,101],[62,9,102],[64,10,103],[66,10,104],[68,10,104],[69,10,105],[71,11,106],[73,11,106],[74,12,107],[76,12,107],[77,13,108],[79,13,108],[81,14,108],[82,14,109],[84,15,109],[85,15,109],[87,16,110],[89,16,110],[90,17,110],[92,18,110],[93,18,110],[95,19,110],[97,19,110],[98,20,110],[100,21,110],[101,21,110],[103,22,110],[105,22,110],[106,23,110],[108,24,110],[109,24,110],[111,25,110],[113,25,110],[114,26,110],[116,26,110],[117,27,110],[119,28,109],[120,28,109],[122,29,109],[124,29,109],[125,30,109],[127,30,108],[128,31,108],[130,32,108],[132,32,107],[133,33,107],[135,33,107],[136,34,106],[138,34,106],[140,35,105],[141,35,105],[143,36,105],[144,37,104],[146,37,104],[147,38,103],[149,38,103],[151,39,102],[152,39,102],[154,40,101],[155,41,100],[157,41,100],[159,42,99],[160,42,99],[162,43,98],[163,44,97],[165,44,96],[166,45,96],[168,46,95],[169,46,94],[171,47,94],[173,48,93],[174,48,92],[176,49,91],[177,50,90],[179,50,90],[180,51,89],[182,52,88],[183,53,87],[185,53,86],[186,54,85],[188,55,84],[189,56,83],[191,57,82],[192,58,81],[193,58,80],[195,59,79],[196,60,78],[198,61,77],[199,62,76],[200,63,75],[202,64,74],[203,65,73],[204,66,72],[206,67,71],[207,68,70],[208,69,69],[210,70,68],[211,71,67],[212,72,66],[213,74,65],[215,75,63],[216,76,62],[217,77,61],[218,78,60],[219,80,59],[221,81,58],[222,82,56],[223,83,55],[224,85,54],[225,86,53],[226,87,52],[227,89,51],[228,90,49],[229,92,48],[230,93,47],[231,94,46],[232,96,45],[233,97,43],[234,99,42],[235,100,41],[235,102,40],[236,103,38],[237,105,37],[238,106,36],[239,108,35],[239,110,33],[240,111,32],[241,113,31],[241,115,29],[242,116,28],[243,118,27],[243,120,25],[244,121,24],[245,123,23],[245,125,21],[246,126,20],[246,128,19],[247,130,18],[247,132,16],[248,133,15],[248,135,14],[248,137,12],[249,139,11],[249,140,10],[249,142,9],[250,144,8],[250,146,7],[250,148,7],[251,150,6],[251,151,6],[251,153,6],[251,155,6],[251,157,7],[252,159,7],[252,161,8],[252,163,9],[252,165,10],[252,166,12],[252,168,13],[252,170,15],[252,172,17],[252,174,18],[252,176,20],[252,178,22],[252,180,24],[251,182,26],[251,184,29],[251,186,31],[251,188,33],[251,190,35],[250,192,38],[250,194,40],[250,196,42],[250,198,45],[249,199,47],[249,201,50],[249,203,53],[248,205,55],[248,207,58],[247,209,61],[247,211,64],[246,213,67],[246,215,70],[245,217,73],[245,219,76],[244,221,79],[244,223,83],[244,225,86],[243,227,90],[243,229,93],[242,230,97],[242,232,101],[242,234,105],[241,236,109],[241,237,113],[241,239,117],[241,241,121],[242,242,125],[242,244,130],[243,245,134],[243,246,138],[244,248,142],[245,249,146],[246,250,150],[248,251,154],[249,252,157],[250,253,161],[252,255,164]]])

def ir_capture(image_queue, calibrate, stop, commands, raw_image_queue):
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

        raw_image_queue.put(copy.copy(im))

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
        self.raw_ir_images = Queue.LifoQueue()

        self.capture_thread = threading.Thread(
                        target=ir_capture,
                        name="capture_thread",
                        args=[self.ir_images, self.ir_calibrate, self.ir_stop, self.ir_commands, self.raw_ir_images]
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
            frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
            self.last_vis_frame = frame
            image = Image.fromarray(frame)

            if not self.overlay:
                self.image_right = ImageTk.PhotoImage(image=image)
                self.image_panel_right.configure(image=self.image_right);

            changed = True

        if not self.ir_images.empty():
            ir_frame = self.ir_images.get()

            if not self.ir_images.empty():
                with self.ir_images.mutex:
                    self.ir_images.queue = []

            ir_image = imutils.resize(ir_frame, height=240, inter=cv2.INTER_LINEAR)
            ir_image = np.dstack((ir_image, ir_image, ir_image))
            ir_image = cv2.LUT(ir_image, colormap).astype('uint8')
           
            self.last_ir_frame = ir_image
            self.image_left = ImageTk.PhotoImage(image=Image.fromarray(ir_image))
            self.image_panel_left.configure(image=self.image_left)
            changed = True

        if changed and self.overlay:
            overlay_image = np.zeros_like(self.last_vis_frame)

            overlay_image[:,:,2] = 0.125 * self.last_vis_frame[:,:,0] + 0.25 * self.last_vis_frame[:,:,1] + 0.125 * self.last_vis_frame[:,:,2]
            converted_frame = cv2.cvtColor(self.last_ir_frame, cv2.COLOR_RGB2HSV)
            overlay_image[:,40:280,2] += 0.5 * converted_frame[:,:,2]
            overlay_image[:,40:280,1] = converted_frame[:,:,1]
            overlay_image[:,40:280,0] = converted_frame[:,:,0]

            overlay_image = cv2.cvtColor(overlay_image, cv2.COLOR_HSV2RGB)

            self.image_right = ImageTk.PhotoImage(image=Image.fromarray(overlay_image))
            self.image_panel_right.configure(image=self.image_right)

        if self.saving:
            if not self.raw_ir_images.empty():
                ir_frame = self.raw_ir_images.get()

                if not self.raw_ir_images.empty():
                    with self.raw_ir_images.mutex:
                        self.raw_ir_images.queue = []
            else:
                ir_frame = None

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
