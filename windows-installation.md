---
layout: splash
classes: wide
---

Here we go. 

# Installing ACL2s on Windows

(N.B. You need about 9GB of free space on your hard drive to install and run the VM.)

  1. Install [VirtualBox version
6.1.16](https://www.virtualbox.org/wiki/Download_Old_Builds_6_1). This is the VM we will be using to run ACL2s. We have tested on version 6.1.16, but it is possible that other Virtualbox 6.1 releases will also work.
  2. Open VirtualBox, and leave it open. 
  3. Download, then install the [VirtualBox extensions for your VirtualBox Version](https://www.oracle.com/virtualization/technologies/vm/downloads/virtualbox-downloads.html#extpack). After downloading the file, you have to actually install the extensions. If you click on `Preferences > Extensions` you will see a green `+` icon that allows you to add extensions. Select the file you downloaded.
  4. Install Vagrant version 2.2.10 ([x86\_64](https://releases.hashicorp.com/vagrant/2.2.10/vagrant_2.2.10_x86_64.msi)) ([i686](https://releases.hashicorp.com/vagrant/2.2.10/vagrant_2.2.10_i686.msi)). 
  5. Open Powershell or another terminal of your choosing; keep it open.
  6. On your machine, from that terminal, create a directory right inside your home folder where you want ACL2s to reside---for instance `acl2s`. Make sure there are no spaces in the full directory name or path. (If you do not understand this step so far, read an introduction to basic unix commands, and then return.) 
  7. Place the following [Vagrantfile]({{ site.baseurl }}/assets/code/Vagrantfile) in that newly-created directory. In your terminal, rename the file to just `Vagrantfile`. We say to do this in an terminal to make sure-sure that you removed the `.txt` extension. 
  8. In this same newly-created directory, create a sub-directory named `workspace`. This will be a synced directory, allowing you to access your ACL2s files outside of the virtual machine. (Read about virtual machines if you do not know what a synced directory is.) Do not enter `workspace`, stay in this same directory.
  9. In the directory we had you create (the one we suggested naming `acl2s`) enter the following command (again, in the terminal you have open)
      
		```bash
		vagrant up
		```

      This will take a while (perhaps up to ~10 minutes) as your computer downloads a collection of files and creates your virtual machine. **Do not close the VirtualBox or move on to the next step until the process is done**. You will know the installation finished when that terminal where you ran `vagrant up` prints your shell prompt again. If you get errors about VT-x being disabled, look see [this post](https://github.com/scotch-io/scotch-box/issues/195). You have to enable virtualization technology in your bios.
  10. Log into the VirtualBox VM window.

		```bash
		login: vagrant
		password: vagrant
		```
  
      If the text in the VM is very small, you can change the scaling of the virtual screen (View -> Virtual Screen 1 -> 200% (or whatever value you want)). You can also use the VirtualBox menu, `Machine > Settings` to change display settings.

  11. After logging in, at the command line type

	   ```bash
	   startxfce4
	   ```

      This will open up a window manager (i.e. "GUI")
  12. Click on `eclipse.sh` on your desktop.
  13. Make sure that you choose the default workspace location so that the synced directories work mentioned above work. 
  14. When you close the VirtualBox machine choose the `Power off machine` option. You have other options, but this is the most robust.
  15. To restart ACL2s later follow the instructions above starting with the `vagrant up` step.

# XServer Option

This is optional, but for those of you who you have an Xserver on your machine, such as [Xming](https://sourceforge.net/projects/xming/), you can run ACL2s using your Xserver instead of logging into the VM.

 1. Type the following commands in an xterm/terminal/powershell from the acl2 directory.

	```bash
	vagrant ssh -- -Y
	eclipse 
   ```

 2. Make sure that you choose the default workspace location so that the aforementioned synced directories. 
 3. When you close the `VirtualBox` machine choose the `Power off machine` option. You have other options, but this is the most robust.
