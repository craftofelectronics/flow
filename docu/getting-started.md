# You Arduino is Safe

You cannot damage your Arduino using Flow. We upload programs, just like you do when you upload from the Arduino environment. We use the same tools to upload code that the Arduino environment uses. If anything happens to your Arduino while you are using Flow, it must (by definition) because you did something silly. "Silly" might include:

1. You dropped your Arduino in a volcano, thus scorching the surface.
1. You plugged your USB cable into an electrical outlet, thus frying your Arduino.
1. You let your 1-year-old use your Arduino as a teething toy, thus destroying both your toddler and your Arduino.

My point: this software cannot hurt your Arduino. Honest. It just uploads programs, just like all the programs you've uploaded before. Except these programs are better.

# Testing Flow

You've downloaded Flow, but does it work?

## Unpack your distribution.

On the Mac, this means drag the application off of the DMG, and put it somewhere.
   
On Windows, this means unzip the .zip file, and put the application somewhere.
   
On Linux, this means un-tar-gzed the file, and put the application somewhere.

## Plug in your Arduino

We recommend using either an Arduino Uno or an Arduino Duemilanove. If your Arduino is equivalent to either of those, make a sensible choice. If you have an Arduino Mega, an Arduino Pro Mini, Flow will not (yet) work for you. It can be made to, but we're just testing at the moment. Join the [users mailing list](http://concurrency.cc/docs/mailinglists.html) at [http://concurrency.cc/](concurrency.cc) and let us know you'd like to see support for another member of the Arduino family if it is really important to you.

Raspberry-Pi support is planned. I'm waiting for mine to come in the mail.

# Run Flow

On Mac and Windows, double-click the "Flow" application. On Linux, open up a command line and run "./flow" from the directory it is in.

## Make some choices

You need to make a few choices:

![images/flow-gui.png](The Flow Launcher)

### Port

You should see your Arduino listed here. If not, try looking in the **Setup** menu and select **Find my Arduino**. If you get stuck at this point, join the users list. (We'll update this documentation if we learn more about how things work on other people's computers). (Note that **Find my Arduino** does not actually know if it has succeeded... so. *Caveat emptor*.)

### Type

At this time, only the Uno and Deumilanove have been tested under Mac (10.6), Windows (XP, 7), and Linux (Ubuntu 10.10 and Fedora "Beefy Miracle").

### Block Set

If you want to just find out if everything is working, choose **Simple**. It only gives you two blocks to play with, that can only be assembled in one obvious way. Otherwise, the **Flow** set has more blocks.

# Launch!

The **Launch** button will open a web browser and open the Flow GUI.

## Testing things with the "Simple" blockset

1. Drag "Read Sensor" and "Fade" out from the left.
1. Wire them up between the connection points.
1. Press Run.

This should send some code to your Arduino. It will read from a sensor and fade an LED based on that sensor's value. **FIXME:** I need to include a block that just blinks pin 13.

## Switching blocksets

To switch blocksets:

1. Flip back to the "Flow" application.
1. Choose a different blockset.
1. Switch back to the GUI (your web browser)
1. Hit "Reload".

Any flows you are working on will be lost at this point. Load and save probably does not work between blocksets... but, I have not tested this. Good luck.

## Programming with the "Flow" blockset

A few more notes, as you explore further.

1. **Wires are point-to-point**, meaning that only one output (the bottom of a block) should connect to one input (the top of a block). We don't detect if you do otherwise, and nothing will happen if you do.
1. **You must supply a username and project name if you want to load and save.** We're using the web service [http://parse.com/](Parse) to store data. It is not on your computer. **It is not private**.

That's all the notes I can come up with at the moment.

# Updates

Flow, at launch, pulls down configuration files from GitHub. In addition, it might try and update the "Flow" library and blocksets that make all the magic happen. No information is gathered about you, your machine, or anything else.

