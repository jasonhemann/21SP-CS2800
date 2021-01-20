---
title: "HW1a"
layout: single
---

### Objectives 
  
  - Set up the technology we will use for this course. 
  - Begin to explore programming in ACL2s.
  - Prepare for the second half of your homework assignment.
  
## After you complete these steps, proceed to the next part of this assignment.

   Find the next part of your assignment [here]({{ site.baseurl}}/assets/code/hw1.lisp).

## Technology Setup and Installation

In the first half of this homework you will set up and configure your
development environment with all the tools you need to fully
participate in class.

  - Follow these instructions and only these. You must follow these instructions to the letter. Some of the installation and configuration is persnickety.
  - If you are not already familiar with basic unix commands, find and read one of the many online tutorials so that you can create directories, rename files, etc. from an `xterm`.
  - We will be using a virtual machine (VM). If you do not know what that is, find and read an introduction to virtual machines.
  - I have listed these instructions in the order you are to  perform them

## 2800 Study Hall / Office Hours Waiting Area
  1. We have already added you to the [2800 Study Hall / Office Hours Waiting Area]. At the top you should find a tab for our course 'Piazza.' Follow the link to Piazza in that tab, and register on Piazza for our course. 


## Piazza 
  1. If you have not already, bookmark [Piazza.com](http://piazza.com/) in your browser, so you can access it outside of Teams. 
  2. Use our Piazza forum to ask for and provide help on the following, should you run into issues. (NB If these or other instructions are missing important information, submit a pull request!)
  3. The top and only post on that Piazza site has a link to the course site on the handins (bottlenose) server. 
   
## Running ACL2s on Khoury Virtual Desktops

You can run ACL2s using the Khoury Virtual Desktops Infrastructure (VDI). See the [documentation](https://www.khoury.northeastern.edu/systems/vdi/). You have many options including using an [HTML client](https://view.khoury.northeastern.edu/portal/webclient/index.html#/) that allows you to log into a virtual machine and run ACL2s using a browser. Use your Khoury CS account credentials and select `CCIS-WINDOWS`; then select `Linux Lab` and you will see a Desktop. Use the file explorer and click on `Other Locations > Computer > bin > acl2s`. Drap the `acl2s` icon to your Desktop while holding "Alt" (on Mac "Option") and when you release it, a menu will pop up; select "link here" and you will have a direct link to ACL2s in your Desktop. Double click on the acl2s icon on your desktop and this will start ACL2s on your Virtual Desktop Machine.

## Hypothes.is 

  5. [Register here](https://hypothes.is/signup) for Hypothes.is, the collaborative editing tool. Our group, for which you should register, is [fa20cs2800](https://hypothes.is/groups/97DiEo3n/fa20cs2800). When we begin our reading assignments, you will wish to take notes with your classmates. 
  
## PollEverywhere

  6. Make sure you can log in to your Northeastern (via your `@northeastern.edu` address) [PollEverywhere account and verify your phone number](https://web.northeastern.edu/nle/poll-everywhere-resources/#menu-item-359:~:text=faculty%2Fstaff-,Students,Verifying%20your%20phone%20number%20if%20you%E2%80%99re%20using%20text%20messages) if you answer PollEverywhere questions via text. 

## Optional - Local ACL2s Installation

  7. Follow the installation instructions for your OS. If something goes wrong, please see the [Uh oh, now what?](#uh-oh-now-what). Needless to say, once you have this working, do not go and update any of that software.

	 - [Linux installation]({{ site.baseurl }}/linux-installation/)
	 - [Windows installation]({{ site.baseurl }}/windows-installation/)
	 - [Mac installation]({{ site.baseurl }}/mac-installation/)
	 
	 If you find Eclipse's resolution insufferable, you can adjust your display settings as described [in the Virtualbox manual](https://www.virtualbox.org/manual/ch03.html#settings-display).

### Uh Oh Now What?

 - My installation failed or seems broken, and I am in a weird state. What should I do?

   First, try running `vagrant destroy` and then `vagrant up`. This will delete your existing VM and create a new one for you. (That's the point of the VM!) Again, this process may take a while. Just start over and go through the instructions carefully. Delete the whole directory with the ACL2s installation and run `vagrant destroy`, and then start again. Make sure to download the `Vagrantfile` again too. 

 - I had everything working just fine. But my machine ran out of battery/crashed/etc, and now when I follow the steps to open ACL2s in Eclipse, Eclipse opens with an error message saying  "Workspace in use or cannot be created, choose a different one." What do I do? 
 
   Eclipse uses a `.lock` file to prevent multiple Eclipse processes from accessing the same workspace. But if you experience a system crash, then it's possible that the `.lock` file did not get removed. Go to your workspace and remove it manually. Please do not remove this file if a working, running, Eclipse process is in fact ongoing. 

 - I am getting error messages about permissions. What should I do?

   Make sure you have administrative permissions so that you can install software on your machine.

 - When I try saving the state of the VM, I get errors when I try to start it up again. Now what?

   Just use the `Power off machine` option. It is more stable and that should resolve your issues.
   
 - VirtualBox is telling me that there is a new version of a box. What should I do?

   The box ubuntu/bionic64 gets updated routinely, but there is no reason for you to upgrade, destroy or recreate the machine. Assuming you have a working installation, there is no need to update unless we explictly tell you to do so.

 - I tried everything and I am getting timeout error messages.

   Make sure that your VM can access the network. A firewall or some other kind of software you have installed may be stopping VirtualBox from accessing the internet. You can check if you have internet access by logging into your VM and typing

   ```bash
   ping google.com
   ```

   If you see timeout messages that means you do not have networking capabilities. How to resolve this depends on your configuration and is not something we can help you with.

 - I am getting errors about VT-x being disabled. What should I do?
   
   You have to enable virtualization technology in your bios. See the Windows installation instructions.
   
