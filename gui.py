from Tkinter import *
import tkFileDialog
import tkSimpleDialog

from poserFile import *


class Utility:
    @classmethod
    def readFrom(cls, filename):
        fp = file(filename)
        result = PoserFile(fp)
        fp.close()
        return result

    @classmethod
    def writeTo(cls, filename, content):
        fp = file(filename, "w")
        content.writeTo(fp)
        fp.close()

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


class OkCancelDialog(tkSimpleDialog.Dialog):
    def __init__(self, parent, title = None, message = None):
        self.message = message
        tkSimpleDialog.Dialog.__init__(self, parent, title)

    def body(self, master):
        l = Label(master, text = self.message, anchor = W, justify = LEFT)
        l.pack()
        return l

    def apply(self):
        self.result = True

def askokcancel(parent, title = None, message = None):
    if OkCancelDialog(parent, title, message).result == True:
        return True
    else:
        return False


class FileField(Frame):

    def __init__(self, master, title = "File to open:",
                 labelwidth = 15, filetypes = (), write = False,):
        Frame.__init__(self, master)
        self.filetypes = filetypes
        self.filename = StringVar()
        self.write = write

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
        if self.write:
            val = tkFileDialog.asksaveasfilename(filetypes = self.filetypes)
        else:
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
        self.figOut = FileField(frame, "Figure file:", filetypes = figuretypes,
                                write = True)
        self.figOut.pack()
        self.injOut = FileField(frame, "Injection file:", filetypes = posetypes,
                                write = True)
        self.injOut.pack()

        Button(frame, text = "Run", command = self.run).pack()

    def run(self):
        if askokcancel(self.master, message = "\n".join([
          "Input figure = " + self.figIn.get(),
          "Output figure = " + self.figOut.get(),
          "Input pose = " + self.injIn.get(),
          "Output pose = " + self.injOut.get()
        ])):
            figure = Utility.readFrom(self.figIn.get())
            pose = Utility.readFrom(self.injIn.get())
            Utility.writeTo(self.figOut.get(), figure)
            Utility.writeTo(self.injOut.get(), pose)


root = Tk()
root.title("pfool's paradise")

app = App(root)

root.mainloop()
