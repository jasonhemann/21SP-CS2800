---
layout: splash
classes: wide
---

Here we go. 

# Installing ACL2s on Mac

(N.B. You need about 9GB of free space on your hard drive to install and run the VM.)

  1. Install [VirtualBox version
6.1.16](https://www.virtualbox.org/wiki/Downloads). This is the VM we will be using to run ACL2s. We have tested on version 6.1.16, but it is possible that other Virtualbox 6.1 releases will also work. (N.B. On recent versions of OSX the installation may fail the first time you try it for lack of sufficient permission.)
    a. If you encounter this failure, go to `System Preferences > Security & Privacy > General` tab and click the `allow` button at the bottom of the screen. This button should have some text to the left of it about Oracle Software. If you can't click on the button, you may need to unlock the settings (click on the lock icon). You may need to do something similar for any software you install.
  2. Open VirtualBox, and leave it open. 
  3. Download, then install the [VirtualBox extensions for your VirtualBox Version](https://www.virtualbox.org/wiki/Downloads#VirtualBox6.1.16OracleVMVirtualBoxExtensionPack). After downloading the file, you have to actually install the extensions. If you click on `Preferences > Extensions` you will see a green `+` icon that allows you to add extensions. Select the file you downloaded.
  4. Download, then install [Xquartz](https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg). 
  5. Open Xquartz and click on `Applications > Terminal` to open the `xterm` program, and keep it open. Use this `xterm` for all the `xterm` commands we ask you to perform below.
  6. Install [Vagrant version 2.2.10](https://releases.hashicorp.com/vagrant/2.2.10/vagrant_2.2.10_x86_64.dmg). 
  7. On your machine, create a directory right inside your home folder where you want ACL2s to reside---for instance `acl2s`. Make sure there are no spaces in the full directory name or path. (If you do not understand this step so far, read an introduction to basic unix commands, and then return.) Putting this directory on your `Desktop` or in your `Documents` or `Downloads` folders can cause problems with permissions in the latest OSX versions! 
  8. Place the following [Vagrantfile]({{ site.baseurl }}/assets/code/Vagrantfile) in that newly-created directory. In your `xterm`, rename the file to just `Vagrantfile`. We say to do this in an `xterm` to make sure-sure that you removed the `.txt` extension. 
  9. In this same newly-created directory, create a sub-directory named `workspace`. This will be a synced directory, allowing you to access your ACL2s files outside of the virtual machine. (Read about virtual machines if you do not know what a synced directory is.) Do not enter `workspace`, stay in this same directory.
  10. In the directory we had you create (the one we suggested naming `acl2s`) enter the following command (again, in the `xterm` you have open)
      
		```bash
		vagrant up
		```

      This will take a while (perhaps up to ~10 minutes) as your computer downloads a collection of files and creates your virtual machine. **Do not close the VirtualBox or move on to the next step until the process is done**. You will know the installation finished when that `xterm` where you ran `vagrant up` prints your shell prompt again.

  11. After this finishes, in the same `xterm` and in the directory we suggested naming `acl2s` (NB not the VirtualBox window), start ACL2s by typing the following commands:

		```bash
		vagrant ssh -- -Y
		eclipse
		```

      If you get errors about not being able to set the display try restarting `Xquartz` and using a new `xterm`. This error may occur when you change wireless networks and those steps can often resolve it. If that fails, you can consider using the [Startx Option](#startx-option). 
  12. Make sure that you choose the default workspace location so that the aforementioned synced directories. 
  13. When you close the `VirtualBox` machine choose the `Power off machine` option. You have other options, but this is the most robust.
  14. To restart ACL2s later follow the instructions above starting with the `vagrant up` step.

# Startx Option

This is optional, but potentially useful if you have persistent display problems with `vagrant ssh`. After following the above installion instructions, to run ACL2s inside the VM you would

  1. Open an xterm. 
  2. Go to the directory we suggested naming `acl2s`. 
  3. If the VM is not running, start it with `vagrant up`. 
  4. Log into the VirtualBox VM window.

		```bash
		login: vagrant
		password: vagrant
		```
  
     If the text in the VM is very small, you can change the scaling of the virtual screen (View -> Virtual Screen 1 -> 200% (or whatever value you want)). You can also use the VirtualBox menu, `Machine > Settings` to change display settings.

  5. After logging in, at the command line type

		```
		startxfce4
		```

     This will open up a window manager (i.e. "GUI")
  6. Click on `eclipse.sh` on your desktop.
  7. Make sure that you choose the default workspace location so that the synced directories work mentioned above work. 
  8. When you close the VirtualBox machine choose the `Power off machine` option. You have other options, but this is the most robust.
