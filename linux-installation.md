---
layout: single
---

## Vagrant

  1. Install [VirtualBox version 6.1.16](https://www.virtualbox.org/wiki/Downloads) either by downloading the installer from the VirtualBox website or using your package manager. We have tested on version 6.1.16, but it is possible that other Virtualbox 6.1 releases will also work.
  2. Open VirtualBox, and leave it open. 
  3. Download, then install the [VirtualBox extensions for your VirtualBox Version](https://www.virtualbox.org/wiki/Downloads#VirtualBox6.1.16OracleVMVirtualBoxExtensionPack). After downloading the file, you have to actually install the extensions. If you click on `Preferences > Extensions` you will see a green `+` icon that allows you to add extensions. Select the file you downloaded.
  4. Install [Vagrant version 2.2.10](https://www.vagrantup.com/downloads) either using your package manager or by using the installer from the Vagrant website. Any version of Vagrant >= 2.2.7 will probably work, but we've only tested 2.2.10.
  5. Create a directory on your machine where you want ACL2s to reside.
  6. Place the following [Vagrantfile]({{ site.baseurl }}/assets/code/Vagrantfile) in that newly-created directory. Ensure that the file doesn't have any extension; it should be just `Vagrantfile`.
  7. In this same newly-created directory, create a sub-directory named `workspace`. This will be a synced directory, allowing you to access your ACL2s files outside of the virtual machine. (Read about virtual machines if you do not know what a synced directory is.) Do not enter `workspace`, stay in this same directory.
  8. In the directory we had you create (the one we suggested naming `acl2s`) enter the following command in a terminal:
      
		```bash
		vagrant up
		```

      This will take a while (perhaps up to ~10 minutes) as your computer downloads a collection of files and creates your virtual machine. **Do not close the VirtualBox or move on to the next step until the process is done**. You will know the installation finished when that terminal where you ran `vagrant up` prints your shell prompt again.

  9. After this finishes, in the same terminal and in the directory we suggested naming `acl2s` (NB not the VirtualBox window), start ACL2s by typing the following commands:

		```bash
		vagrant ssh -- -Y
		eclipse
		```

      If you get errors about not being able to set the display, you may need to install `xauth` using your package manager (`xauth` on Ubuntu, `xorg-xauth` on Arch, `xorg-x11-xauth` on Fedora). If that fails, you can consider using the [Startx Option](#startx-option). 
  10. Make sure that you choose the default workspace location so that the aforementioned synced directories. 
  11. When you close the `VirtualBox` machine choose the `Power off machine` option. You have other options, but this is the most robust.
  12. To restart ACL2s later follow the instructions above starting with the `vagrant up` step.


# Startx Option

This is optional, but potentially useful if you have persistent display problems with `vagrant ssh`. After following the above installion instructions, to run ACL2s inside the VM you would

  1. Open a terminal.
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
