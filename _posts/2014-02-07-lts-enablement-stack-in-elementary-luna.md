---
layout: post
title: LTS enablement stack in elementary Luna
---

Last night I upgraded the kernel and graphics stack on my luna 64-bit installation but ran into some problems.

After rebooting, X wouldn't start but that was because I didn't do a 
{% highlight bash %} sudo update-grub {% endhighlight %}
from my Ubuntu 13.10 installation (which I dual boot).

Woke up, turned on my computer to find eOS would go through the splash screen, then nothing. Solved that by running
{% highlight bash %} sudo dpkg-reconfigure xserver-xorg-lts-saucy {% endhighlight %}

Now it's running beautifully!