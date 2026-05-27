import sys
import random
from PySide6 import QtCore, QtWidgets, QtGui
from PySide6.QtWidgets import (
    QApplication, QMainWindow, QGraphicsView, QGraphicsScene,
    QGraphicsRectItem, QGraphicsEllipseItem, QPushButton, QWidget,
    QVBoxLayout, QLabel
)
from PySide6.QtGui import QPen, QBrush
from PySide6.QtCore import Qt, QRectF, Slot, QObject, Signal


#first QtWidgets application
"""
app = QApplication(sys.argv) #creates the application
label = QLabel("Hello world!") #label that exhibits "Hello world!"  
label.show() #command to show the label
app.exec() #command to run the application
"""

#buttons: signals and slots
"""
@Slot()
def say_hello():
    print("Button clicked, Hello!")

def function():
    print("The 'function' has been called!")
    
app = QApplication(sys.argv)
button = QPushButton("Click me") #creates a button
button.clicked.connect(say_hello) #connects the button to the "say_hello()" function
button.show() #shows the button

button1 = QPushButton("Call function")
button1.clicked.connect(function)
button1.show()
sys.exit(app.exec())
#app.exec() #runs the application
"""

#Signals and slots
#
# Signals can be seen as event notifications emitted by Qt objects (they are usually described using
# classes).  When something relevant happens to an object — for example, a button is clicked — the
# object emits a signal.
#
# Slots are functions or methods connected to signals.  When the signal is emitted, Qt automatically
# calls the connected slot, causing the corresponding action to run.

class Communicate(QObject):
    # create two new signals on the fly: one will handle
    # int type, the other will handle strings
    speak = Signal((int,), (str,))

    def __init__(self, parent=None):
        super().__init__(parent)

        self.speak[int].connect(self.say_something)
        self.speak[str].connect(self.say_something)

    # define a new slot that receives a C 'int' or a 'str'
    # and has 'say_something' as its name
    @Slot(int)
    @Slot(str)
    def say_something(self, arg):
        if isinstance(arg, int):
            print("This is a number:", arg)
        elif isinstance(arg, str):
            print("This is a string:", arg)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    someone = Communicate()

    # emit 'speak' signal with different arguments.
    # we have to specify the str as int is the default
    someone.speak.emit(10)
    someone.speak[str].emit("Hello everybody!")
