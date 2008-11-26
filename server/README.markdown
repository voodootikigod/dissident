What is Dissident?
==================

Dissident is a brand new web application framework designed from the ground up to solve most of the problems plaguing other framework by simply looking at the past. The project will stick to its namesake by going right where others went -wrong- left. For discussion and questions please use the IRC room #dissident on irc.freenode.net or the github wiki for the project at http://github.com/voodootikigod/dissident/wikis. Here is the general concept of the framework:

Backend
-------

Using Erlang on the backend will allow the system to fully utilize a single system as well linearly and massively scale up to a huge cluster as the demand and need arise. Furthermore due to that nature of Erlang, the system can be highly fault tolerant and straddle numerous geographically distant hosting providers and hosting provider boundaries. The backend system is currently targeted to include the following components:

* mnesia as the persistent store (look ma’ no ORM)
* mochiweb as a lightweight HTTP server
* webmachine as "a layer that adds HTTP semantic awareness on top of the excellent bit-pushing and HTTP syntax-management provided by mochiweb" -- http://blog.therestfulway.com/.
* Your webmachine resources - http://code.google.com/p/webmachine/wiki/WebmachineResources

Through these three technologies, developers will be able to build a full API that can scale across multiple boxes. The concept of Dissident is to isolate logical pieces so they can be tested and reused without modification. The entire backend is simply building a usable RESTful Web Service THAT DOES NOT dump to a template engine for pushing content, and instead delivers data via REST requests and JSON response. Nothing magical, nothing special, nothing you wouldn’t provide to any of your 3rd parties right? Except you have to eat your own dog food when it comes to Dissident. Your API is what will drive your entire User Interface so you have to build it right, you are forced to not "cut corners" and make your application such that it is testable, repeatable, and documentable.

Front End
---------

The front end is split in to the typical trinity for web development, style (CSS), structure (HTML), and behavior (JS). Only in the case of Dissident we are embracing the modern web world instead of rehashing old stuff. With Javascript and AJAX engines being powerhouses these days, we are going to put them to the test. Dissident utilizes the AJAX Head design pattern as its core mantra for UI development. Designers simply generate CSS and HTML with minimal “single instance” templates (render a single item) built in; those are served directly to the browser. Doing this eliminates the need for caching, optimizes the system for CDN delivery, and removes all the pain (and inability to test) associated with co-mingled views. JQuery (current selection) is employed to query back to the API backend and obtain and render the dynamic elements of the site. In this manner the structure can be verified independently of the dynamic rendering within the HTML, thus allowing you 100% testability. Furthermore since all of the “requesting” for dynamic data is handled through JQuery, it can easily be updated through AJAX requests, which requires a server to be able to handle a large amounts of requests (thanks Erlang). Oh also, wouldn’t you know it, mochiweb gives us the ability to do Comet interactions too, so Dissident can push updates instead of constant polling.


The cool things you might have missed
-------------------------------------

As part of the Javascript, Dissident will generate a dynamic config for session based on where they requestor is coming from with the 2-3 nodes closest (similar to how Rails does the caching and round robin assets hosting for js and css) and provides the ability to identify an “external” node that can be used as a failover, ideally on a different landmass or hosting provider. So lets say you have Dissident on a US EC2 and it spans out to a Blue Box Group or EU EC2 back up, if the US EC2 goes down, your site transitions over to the failover automatically. It stays alive fluidly without breaking sessions.

I can’t stress the main point the most, within Dissident designers design, coders code. There is no templating, no crappy untestable views, everything is 100% automated testable (TATFT) and everything is fault tolerant.

Or at least it will be when it is built.