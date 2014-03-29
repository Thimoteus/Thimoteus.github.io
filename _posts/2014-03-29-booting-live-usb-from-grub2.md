---
layout: post
title: Booting a Live USB From Grub2
---

For some reason, my computer will boot from the hard drive even if I change the boot order in the EFI menu.
This is why I wanted to be able to boot to a live USB from Grub 2, especially when a new version of a distro I use comes out (I prefer clean installs).

Assuming your USB is detectable, go to the grub command line from the menu by pressing `c`.

Use `ls` to determine where your USB is.
In my case, it was `(hd0,1)`. (Note: in the following, you should uniformly replace occurrences of `(hd0,1)` with the location you determined from above).
You can tell by typing, for example, `ls (hd0,1)/`.

Then use `set root=(hd0,1)`.

Now you need to figure out where the boot loader on the USB is.
You can do this by more `ls` commands; in my case, it was in `(hd0,1)/EFI/BOOT/`.

Then `chainloader (hd0,1)/EFI/BOOT/grubx64.efi` or whatever the boot loader filename is.

Finally `boot` and you're done!