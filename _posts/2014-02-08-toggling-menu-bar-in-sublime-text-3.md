---
layout: post
title: Toggling the menu bar in Sublime Text 3
---

Today I ran into a strange bug: there wasn't any way to hide the menu bar in Sublime Text 3.

According to [this thread](http://sublimetext.userecho.com/topic/48628-hideshow-menu-bar-shortcut/),
it should be possible to ctrl+alt+p and find "toggle menu" but that wasn't the case for me.

There was also no shortcut to toggle the menu in the key bindings files, so I made my own:

Go to Preferences > Key Bindings - User, and add the following:

{% highlight json %}
	{ "keys": ["ctrl+alt+m"], "command": "toggle_menu" }
{% endhighlight %}

Make sure that ctrl+alt+m isn't bound to anything, or use a keybind that isn't yet bound. Save the file.

That's it!