Jan 2002, Jörgen Viksell - jorgen.viksell@telia.com,
          Jorge Arellano Cid --
Last update: Jan 2021, Kamil Ignacak


==================
 Cookies in Dillo
==================

The current specification for cookies is RFC 6265
( http://tools.ietf.org/html/rfc6265 ).

Cookies are handled by a dpi (plugin) which shares them between your
instances of Dillo.

Current cookie limits are: 20 per domain, and 1200 in total.

When the dpi exits, cookies that you have ACCEPTed are saved to
~/.dillo/cookies.txt, and ACCEPT_SESSION cookies are forgotten.
The dpi normally exits after a period of inactivity, but you can force it to
exit with the command "dpidc stop".


=====================
 Controlling cookies
=====================

Out of the box, dillo rejects all cookies.


If you want to accept certain cookies, you can specify rules for different
domains in the file ~/.config/.hello/cookiesrc. The syntax looks like:

#host         action
DEFAULT       DENY
fltk.org      ACCEPT
.host.com     ACCEPT_SESSION

Line 0: Comment line begins with '#'.
Line 1: Deny all cookies from all domains not otherwise specified.
Line 2: Accept all cookies from fltk.org, and save them to
        ~/.dillo/cookies.txt when the cookies dpi exits.
Line 3: Accept all cookies from all subdomains of host.com, but
        do not save them when the dpi exits.

If the file does not exist, it is created by the program with this contents:
DEFAULT DENY

The program does not care about case of strings in this file, but for
historical reasons ("this is how dillo did it") the action strings and the
"DEFAULT" string are upper-case.

Config file lines with two tokens are accepted for further parsing.

Config file lines with one, three or more tokens are discarded. If such line
contained a valid domain, the domain is discarded, and default global action
will be used for that domain.

Domains, for which action is malformed in the config file (e.g. "DEN",
"ACC_EPT", "OK"), are added to config, and the DENY action is
assigned. Mixed-case action string (e.g. "Deny" or "AccePT") is not
considered to be malformed.

The program stores the rules in a container. The rules must be stored as
ordered by domain length, with longest first, so the first match is the most
specific.

Domains stored in the container are always lower-case.

Domains passed to module's "lookup" function are always forced to lower-case
before action lookup takes place.


===================
 Cookies & Privacy
===================

 Cookies can be a severe threat to personal privacy. The pages you
visit can be tracked, logged, and associated to a peronal data-record,
allowing the possibility of building a detailed profile of your
browsing habits.

 This data is sold to companies that profit from direct use of such
information (SPAM, Spying, etc).

 If this data is cross-referenced with other databases, they can end up
with more information than you have about yourself.

 Some people may tell you this is "paranoid". But please, take my words
as those of someone who has written a web browser, a cookies implementation,
and who has deep understanding of HTTP and cookies.

 The dillo project is especially concerned about privacy and security
issues. Our advice is to avoid cookies whenever possible and at most set
ACCEPT_SESSION to specific, trusted sites.  -- You have been warned.

