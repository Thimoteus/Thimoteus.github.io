---
layout: post
title: Removing empathy in elementary luna
---

Empathy doesn't seem to integrate well into elementary luna. I noticed new messages didn't highlight
the message indicator in wingpanel, but they do in pidgin. Problem is having pidgin and empathy in
the message indicator is a little redundant. Most people try a

{% highlight bash %} sudo apt-get purge empathy {% endhighlight %}

but this doesn't remove empathy's integration in the messaging menu.

For that you need to run

{% highlight bash %} sudo apt-get purge telepathy-indicator {% endhighlight %}

and restart wingpanel (or log out then in). Voila! No more empathy or empathy indicator.