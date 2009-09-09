from Tkinter import *
import tkFileDialog
import tkSimpleDialog


class MessageDialog(tkSimpleDialog.Dialog):
    def __init__(self, parent, title = None, message = None):
        self.message = message
        tkSimpleDialog.Dialog.__init__(self, parent, title)

    def body(self, master):
        l = Label(master, text = self.message, anchor = W, justify = LEFT)
        l.pack()
        return l

    def buttonbox(self):
        box = Frame(self)

        w = Button(box, text = "OK", command = self.ok, default = ACTIVE)
        w.pack(side = LEFT, padx = 5, pady = 5)

        self.bind("<Return>", self.ok)

        box.pack()


class FileField(Frame):

    def __init__(self, master,
                 title = "File to open:", labelwidth = 15, filetypes = ()):
        Frame.__init__(self, master)
        self.filetypes = filetypes
        self.filename = StringVar()

        Label(self,
              text = title, width = labelwidth, anchor = W, justify = LEFT
              ).pack(side = 'left')
        Entry(self,
              width = 40, textvariable = self.filename, background = "white"
              ).pack(side = 'left')
        Button(self,
               text = "Browse", command = self.browse
               ).pack(side = 'left')

    def browse(self):
        val = tkFileDialog.askopenfilename(filetypes = self.filetypes)
        if val:
            self.filename.set(val)

    def get(self):
        return self.filename.get()


class App:

    def __init__(self, master):

        self.master = master
        frame = Frame(master, borderwidth = 10)
        frame.pack()

        figuretypes = (("All files", "*"), ("Figure files", ".cr2"))
        posetypes = (("All files", "*"), ("Pose files", ".pz2"))

        Label(frame, text = "Inputs:").pack()
        self.figIn = FileField(frame, "Figure file:", filetypes = figuretypes)
        self.figIn.pack()
        self.injIn = FileField(frame, "Injection file:", filetypes = posetypes)
        self.injIn.pack()

        Label(frame, text = "Outputs:").pack()
        self.figOut = FileField(frame, "Figure file:", filetypes = figuretypes)
        self.figOut.pack()
        self.injOut = FileField(frame, "Injection file:", filetypes = posetypes)
        self.injOut.pack()

        Button(frame, text = "Run", command = self.run).pack()

    def run(self):
        MessageDialog(self.master, message = "\n".join([
          "Input figure = " + self.figIn.get(),
          "Output figure = " + self.figOut.get(),
          "Input pose = " + self.injIn.get(),
          "Output pose = " + self.injOut.get()
        ]))


root = Tk()
root.title("pfool's paradise")

app = App(root)

root.mainloop()
