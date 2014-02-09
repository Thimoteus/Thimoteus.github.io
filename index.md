---
layout: default
title: The natural order of things
---

<dl>
	{% for post in site.posts %}
		<dt><a href="{{ post.url }}">{{ post.title }}</a></dt>
		<dd>
			{{ post.excerpt }}
		</dd>
	{% endfor %}
</dl>