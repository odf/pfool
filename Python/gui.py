from Tkinter import *
import tkFileDialog
import tkSimpleDialog

from poserFile import *


class EchoDict(dict):
    def get(self, key):
        if dict.has_key(self, key):
            return dict.get(self, key)
        else:
            return key

    def __getitem__(self, key):
        if dict.has_key(self, key):
            return dict.__getitem__(self, key)
        else:
            return key


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
            # -- read input files
            figure = Utility.readFrom(self.figIn.get())
            pose = Utility.readFrom(self.injIn.get())

            # -- process
            self.resolveConflictingChannelNames(figure, pose)
            self.injectChannels(figure, pose)

            # -- write output files
            Utility.writeTo(self.figOut.get(), figure)
            Utility.writeTo(self.injOut.get(), pose)

            MessageDialog(self.master, message = "Done!")

    def resolveConflictingChannelNames(self, figure, pose):
        # -- collect channel names present in figure
        usedChannels = set()
        for node in figure.root.select("actor", "channels",
                                       "targetGeom|valueParm"):
            usedChannels.add(node.rest)

        # -- determine new names for conflicting channels in pose
        old2new = EchoDict()
        for node in pose.root.select("actor", "channels",
                                     "targetGeom|valueParm"):
            name = node.rest
            if name in usedChannels and not old2new.has_key(name):
                for i in xrange(1000):
                    newName = "xtra%03d" % i
                    if newName not in usedChannels:
                        old2new[name] = newName
                        usedChannels.add(newName)
                        break
                else:
                    raise "That's too many channels, dude!"

        # -- change names in channel definitions
        for node in pose.root.select("actor", "channels",
                                     "targetGeom|valueParm"):
            node.rest = old2new[node.rest]

        # -- change names in dependent parameter (ERC) instructions
        for node in pose.root.select("actor", "channels", ".*", "valueOp.*"):
            source = node.nextSibling.nextSibling.nextSibling
            source.firstField = old2new[source.firstField]

        # -- change names in dial groups
        for node in pose.root.select("actor", "channels", "groups"):
            for desc in node.descendants:
                if desc.firstField == "parmNode":
                    desc.rest = old2new[desc.rest]

        # -- change names in parameter linking instructions
        for node in pose.root.select("figure", "linkParms"):
            parm = node.nextSibling
            parm.text = old2new[parm.text]
            parm = parm.nextSibling.nextSibling
            parm.text = old2new[parm.text]

    def injectChannels(self, figure, pose):
        for node in pose.root.select("actor"):
            actor = figure.actor(node.rest)
            if not actor:
                continue
            for ch in actor.content.select("channels", ".*"):
                if not ch.firstField in ('{', 'groups'):
                    anchor = ch
                    break
            for ch in node.select("channels", "targetGeom|valueParm"):
                anchor.prependSibling(ch.clone())

        
root = Tk()
root.title("pfool's paradise")

app = App(root)

root.mainloop()
