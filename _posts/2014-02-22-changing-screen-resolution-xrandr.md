---
layout: post
title: Changing Screen Resolution With xrandr in Luna
---

First use `xrandr` and get output similar to:

{% highlight bash %}
Screen 0: minimum 320 x 200, current 1600 x 900, maximum 32767 x 32767
LVDS1 connected primary 1600x900+0+0 (normal left inverted right x axis y axis)
{% endhighlight %}

In this case the output is `LVDS1`. If we want to move from 1600x900 to, say, 1920x1080, we need to figure out the scaling constant:

{% highlight bash %}
$ echo "1920/1600" | bc -l
1.20000000000000000000
{% endhighlight %}

Then to bump up the screen resolution:

{% highlight bash %}
$ xrandr --output LVDS1 --mode 1600x900 --scale 1.2x1.2 --panning 1920x1080
{% endhighlight %}

To reverse it:

{% highlight bash %}
$ xrandr --output LVDS1 --mode 1600x900 --scale 1.0x1.0 --panning 1600x900
{% endhighlight %}