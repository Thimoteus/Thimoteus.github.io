---
layout: post
title: Github Pages, Freedns and Freenom
---

I've always wondered how close you could get to having your own [TLD](http://en.wikipedia.org/wiki/Top-level_domain) and hosting for free. As of yesterday I managed to get www.thimoteus.tk (previously thimoteus.github.io) and have all the redirects working as I wanted (so far) thanks to [freedns](http://freedns.afraid.org/).

There is hardly any documentation, if there is any at all, about making these three specific services work with each other. For a few hours yesterday I wasn't sure what came first: registering the domain name, setting something up with freedns, or committing a CNAME file to my github pages repository.

As far as I can tell, .tk domains are all registered and managed by the same company (also located in Amsterdam to my surprise)---maybe they have a contract with the government of Tokelau. What confused me during the registration process was configuring the domain to point to somewhere else. You can use their own, very basic, DNS service, or your own. This is where freedns comes in.

On the freedns site, there are instructions to only use the nameservers `nsX.afraid.org` for $$X \in \{1,2,3,4\}$$, so that's what I did when registering the domain. After that it wasn't much time until thimoteus.tk was showing some generice freedns page. Awesome.

Then came the problem of pointing the domain to my Github page. Unfortunately the documentation on Github pages for custom domains is scattered over too many articles, but it wasn't too difficult. All I did was commit a file named `CNAME` with the single line `www.thimoteus.tk` in the body to the root directory of my git project (in this case `www` being the subdomain), then manage the subdomains on freedns to reflect that: edit the www.thimoteus.tk subdomain, choose a CNAME type to point www.thimoteus.tk to thimoteus.github.io, then save and wait for a few hours.

This is all good and fine, but the last hurdle was trying to send thimoteus.tk to the www subdomain. Most of the documentation I found referred to an `ALIAS` type, which freedns doesn't have (at least by name). But it turns out that a simple web forward will work here, that way thimoteus.tk will always redirect to www.thimoteus.tk (and it will show that on the browser's address line).