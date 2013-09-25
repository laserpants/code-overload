** Step 1: Initialization
Download [https://github.com/angular/angular-seed][Angular Seed] project to use as a base.  I'm not totally in love with their organization but it is probably the most commonly used folder structure in the Angular world.  So for now we'll stick with it.

** Step 2: Cleanup
Made simple index.html file for single view.
Added basic bootstrap css and fonts.
Removed sample controllers, directives, filters, and services.
Added controller stubs we will actually use.
Added very basic partial templates for the 3 main app operations: list all, view one, view version.

*** Step 3: Load local json
Added a resource for loading local, static json files with sample snippet data.

*** Step 4: Load remote json
Got snippet and comment data loading from a remote server.
Escaped the port number in the server url. (There was a problem as the normal https://server:port/path/:param url was replacing/ignoring the :port part.  Using server\\:port fixed the problem.)
Updated the templates to use the real data format from the server.

** Step 5: Beautify templates
