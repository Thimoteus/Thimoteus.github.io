---
layout: post
title: Using facebook chat with weechat and bitlbee
---

I've been trying to find a good client for facebook chat that isn't switching to my browser window,
then to the facebook tab, then focusing on the chat window of whoever-I'm-talking-to, and typing.

I remember having bad experiences with empathy (even though it's a default app in elementary OS)
so I started using [fbmessenger][fbmessenger] for a while but it tends to bug out afer extended
(read: a few hours) use.

Eventually I wanted to find out if there's any way of using jabber with IRC, since there's an
"elementarified" IRC app in [Cable][cable].

That's when I found out about [bitlbee][bitlbee].

I didn't manage to get Cable working with bitlbee at first, so I downloaded [weechat][weechat].

Follow the "install your own server" instructions [here](http://wiki.bitlbee.org/GettingStarted):

{% highlight bash %} sudo apt-get install bitlbee {% endhighlight %}

then edit /etc/bitlbee/bitlbee.conf to have

{% highlight bash %}
DaemonInterface = 127.0.0.1
DaemonPort = 6667
{% endhighlight %}

and restart the bitlbee daemon:

{% highlight bash %} sudo /etc/init.d/bitlbee restart {% endhighlight %}

Then to create the bitlbee server on weechat,

{% highlight bash %} /server add bitlbee localhost {% endhighlight %}

Add a few server options:

{% highlight bash %}
/set irc.server.bitlbee.nicks "nick name"
/set irc.server.bitlbee.username "user name"
/set irc.server.bitlbee.realname "real name"
/set irc.server.bitlbee.autoconnect on
/set irc.server.bitlbee.autojoin "&bitlbee"
{% endhighlight %}

Finally to connect to bitlbee:

{% highlight bash %} /connect bitlbee {% endhighlight %}

Now we need to register with the server using a password.

{% highlight bash %} register password {% endhighlight %}

When you connect to bitlbee next time (remember, one of the config options above makes it
so whenever you start weechat, you're connected to bitlbee) you'll use the identify command
instead of register.

The [HowToFacebook](http://wiki.bitlbee.org/HowtoFacebook) page has a handy guide on how to
connect. Unfortunately, it says OAuth is only available from version 3.0.5+ and the one in
the repos is 3.0.4. That means we have to connect the old fashioned way, with a password:

{% highlight bash %} account add jabber yourusername@chat.facebook.com password {% endhighlight %}

where password is your facebook password.

Now on the side you should see your online contacts, but their names aren't recognizable. Fix that with

{% highlight bash %} account fb set nick_format %full_name {% endhighlight %}

You might also want to create a [list](http://wiki.bitlbee.org/UiFix) of offline accounts. That's easy:

{% highlight bash %}
/join &offline
chan set show_users offline
{% endhighlight %}

Then to quit, simply type /quit.

A handy list of commands:

{% highlight bash %}
Ctrl + Alt + Arrow		# change viewport to the left or right
/query contactName		# send a new IM to contactName
set offline_user_quits='false'	# cuts down on some online/offline spam
/buffer close n			# where n is the # of the buffer to close
{% endhighlight %}

That's it for now!

Next I will try to get bitlbee and weechat to work with notify osd. There seem to be a lot of notification
scripts [here](http://weechat.org/scripts/stable/tag/notify/) ...

<!-- references -->

[fbmessenger]: http://www.webupd8.org/2013/04/fbmessenger-stand-alone-facebook.html "facebook messenger"
[cable]: http://linuxg.net/how-to-install-cable-irc-client-on-ubuntu-13-10-13-04-12-04-linux-mint-15-13-and-elementary-os-0-2-luna/ "cable"
[bitlbee]: http://wiki.bitlbee.org/ "bitlbee"
[weechat]: http://weechat.org "weechat"