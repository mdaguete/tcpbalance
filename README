
TCPBalance, a load-balancing TCP proxy for distcc
=================================================

There are dozens of Open Source TCP proxies available, written in
close to a dozen languages, many of them capable of load balancing.
Many of them would work with "distcc".  Why write yet another TCP
proxy?  Why do it in Erlang?

All of the TCP proxies I found, none appeared to have the following
combination of features:

    1. Not be too HTTP-centric to not be able to work with "distcc".

    2. Be aware that some back-end hosts may be faster than other
       hosts.  For each client connection, the proxy should choose the
       fastest back-end host that is currently idle.

    3. Be aware of back-end hosts with multiple CPUs.

    4. When all back-end hosts are busy, make the client wait for the
       next available back-end host when it is available, rather than
       giving a back-end host more work than it is configured to
       handle.

    5. Detect when a back-end host is down and do something sane (like
       avoid giving future jobs to the dead machine).  

    6. Permit an administrator to put back-end hosts back in service,
       take them out of service, as well as add and remove hosts from
       the pool without adversely affecting clients using the proxy.

    7. Keep basic statistics about back-end hosts and make them
       available via HTTP or Telnet.

Features 1-5 were mandatory.  Features 6-7 would be nice.  In a couple
of hours of Web surfing, I didn't find a TCP proxy that was capable of
doing 1-5, so I decided to write my own.

I knew it would be fairly easy to implement features 6-7 as well as
1-5 in Erlang (see http://www.erlang.org/), so that's what I used.

This proxy has been in use at Caspian Networks for over two months.
It's pretty solid.

This README file is quite long.  Sorry about that.  However, much of
it is a tutorial for Erlang newbies ... and perhaps a bit of
evangelism.  :-)  I'll try to keep things straightforward, but I will
also demonstrate some of the nifty communication, fault-tolerance, and
hot code upgrade features of Erlang.

License
-------

See the file "LICENSE", at the top of the tcpbalance source
distribution, for full licensing terms.


Obtaining, Compiling, and Installing Erlang
-------------------------------------------

Oh boy, yet another programming language development environment to
compile and install.  Well, if you're still reading this, you're
interested enough in tcpbalance's feature set to try it out.

The current release of Erlang (as of 15 January 2003) is R9B-0.  All
of the source and pre-compiled packages mentioned below can be
obtained from http://www.erlang.org/download/.

* UNIX and Linux users:

Erlang runs quite well under: Linux, BSD flavors, Solaris, and
others.

Although it's slow, I recommend obtaining the source and compiling
from scratch.  The distribution is pretty big (8MB compressed) and
contains a kajillion files ... but the Erlang programming & runtime
environment contains many useful tools (including a nearly-complete
CORBA implementation) that are themselves written in Erlang.
Tcpbalance doesn't use most of them, but you get to compile all of
them.  :-)

Follow the included directions, but the simple instructions are:

    1. Extract the source package and change directory to its root.
    2. Run "./configure" to use the default installation root
       ("/usr/local" on most platforms) or
       "./configure --prefix=/path/to/install/root" if you wish to use
       another installation root path.
    3. Run "make".
    4. Go do something else for a while.  The Erlang VM is implemented
       in C, which compiles in a few minutes, but most of the run-time
       environment & misc tools are implemented in Erlang itself, and
       there's a *lot* of it.
    5. Run "make install".
    6. Add /path/to/install/root/bin, whatever it is, to your shell's
       program path.

* MacOS X users:

I haven't used it, but there's a pre-compiled, disk image-style
installation thingie available.  Give it a try if you wish.

* Microsoft Windows users:

Your only option is installing a pre-compiled package.  Whoo hoo!

When I installed Erlang R9B-0 on a Windows NT 4.0 machine, I
discovered that "C:\Program Files\erl5.2\bin" was already added to my
program path.  What a deal.


Testing Erlang Inter-Node Communication
---------------------------------------

One of the really nice things about Erlang is that communication (via
message passing) between threads inside an Erlang virtual machine is
exactly the same as message passing between threads on different
Erlang virtual machines.

An instance of the Erlang virtual machine is called a "node".  Erlang
threads are called "processes", which can really confuse UNIX geeks if
you're not careful.  In this document, if I want to refer to a UNIX or
NT process/task, I'll call it an "operating system process" or "OS
process" to avoid confusion.

Erlang's inter-node message passing relies on a simple shared secret
mechanism similar to the X11's "MIT magic cookie" authentication
scheme.  Erlang stores a "cookie" in $HOME/.erlang.cookie (where $HOME
is your account's home directory) on UNIX boxes and in
C:\.erlang.cookie on NT boxes.  To communicate with each other, all
Erlang nodes must share the exact same cookie.

(There are other ways to configure cookies, but I'm only going to
 describe one.)

For nodes running on the same machine, there's no problem: everybody
is sharing the same file system(s).

For nodes running on different UNIX machines, you'll need to:
    1. Use NFS or another shared file system for your $HOME directory.
    2. Copy the .erlang.cookie file by hand to the $HOME directory of
       each machine you wish to run an Erlang node.

For nodes running on different NT machines, you'll need to copy the
.erlang.cookie to each machine.

If you're mixing NT and UNIX machines (which is certainly possible),
make certain the exact same .erlang.cookie file is used on all of
them.

To make life easier on yourself, make certain that all of the machines
involved are present in DNS.  E.g. if you're playing with "davinci"
and "munch", make certain that "ping davinci" and "ping munch" works
on both machines.

If a machine has a hyphen, "-", in its DNS name ... choose another
machine.  Erlang's syntax requires treating hyphens specially.  This
example is complicated enough as it is.  (See footnote [1] below.)

An Erlang node name looks like:
    foo@hostname
or:
    foo@hostname.fully.qualified.domain
For simplicity, we'll use the former.

Each node must have a unique node name.  For nodes running on the same
machine, the lefthand side of the "@" must, therefore, be unique.  If
you attempt to start two nodes on the same machine with the same name,
the second VM will spit out a very long and cryptic error message
(including the string "Kernel pid terminated").

For my example, I'm going to use the nodes 'foo@davinci' and
'bar@rover'.

NOTE: Erlang commands are case-specific!

On each machine, start the Erlang VM and interactive shell:
    1. UNIX: run "erl -sname NODE_LHS".	 E.g. "erl -sname foo"
    2. NT: run "werl -sname NODE_LHS"    E.g. "werl -sname foo"
    3. Mac OS X: I've never used it, so you're on your own.

You will see something like:

    Erlang (BEAM) emulator version 5.2 [source] [hipe] [threads:0]
    
    Eshell V5.2  (abort with ^G)
    (foo@davinci)1> 

Note that the node name is included in the prompt string.

Type the command "erlang:get_cookie()." and press Enter.  You should
see:

    (foo@davinci)1> erlang:get_cookie().
    'JQHZIQLDNQGUSZRAJXHB'

You should see the same cookie on each node.  If not, go back and fix
it.

Now, we'll test the inter-node message passing capability.  One one of
your nodes, type the following:

    (foo@davinci)2> register(test, self()).
    true
    (foo@davinci)3> receive Msg -> Msg end.

You won't get see a prompt right away: you're blocked waiting for a
message to arrive.

On the other machine, type the following, substituting the other
machine's node name:

    (bar@rover)1> {test, 'foo@davinci'} ! hello_world.
    hello_world

On the first machine, you should see:

    (foo@davinci)3> receive Msg -> Msg end.
    hello_world
    (foo@davinci)4>

If you run the function "nodes()", you should see the name of all
other nodes that your node is aware of.

    (bar@rover)3> nodes().
    [foo@davinci]
    (bar@rover)4> 

Congratulations!  It's now time to play with the tcpbalance
application.  Use the command "q()." to exit the shell, or press
Control-c.


Modifying the application and Web server config to fit your environment
-----------------------------------------------------------------------

I haven't spent any time trying to auto-magically edit the files that
will need editing before you can use tcpbalance.  I've made life
"easier" by using relative, not absolute paths.  Therefore, no extra
configuration or file editing should be necessary.

This means that you *must* change the current working directory
exactly as described below (or else things won't work).  This is not
how a "real" Erlang application would be installed & run, but I
haven't taken the time to do that.  Sorry.

If you want the built-in HTTP server to use a port other than port
8080, edit the file "priv/inets.conf" and modify the "Port" directive.


Compiling the application
-------------------------

The file "src/Makefile" requires GNU Make.  To use it:

    % cd src
    % make

If you do not have GNU Make, or if you're using NT and don't have GNU
Make available, execute the commands found in the file "Make.all.out".

The file src/balance.rel contains version numbers that are specific to
a particular Erlang/OTP release version.  If you see error messages
like this:

    stdlib: No valid version ("1.11.0") of .app file found. Found file
    "/usr/local/lib/erlang/lib/stdlib-1.11.4.1/ebin/stdlib.app" with
    version "1.11.4.1"
    
    kernel: No valid version ("2.8.0") of .app file found. Found file
    "/usr/local/lib/erlang/lib/kernel-2.8.1.1/ebin/kernel.app" with
    version "2.8.1.1"
    
    gmake: *** [balance.boot] Error 1

... then you have a different version of Erlang/OTP than I originally
used for tcpbalance.  Edit the file src/balance.rel to replace the
invalid version numbers with the version numbers mentioned at the end
of each error message.


Running the application with the example configuration file
-----------------------------------------------------------

Run the following:

    % cd src                 ... if you haven't already
    % erl -sname bal -pz ../ebin -boot balance -config ../priv/be-list -noshell

You will see a whole bunch of diagnostic messages, labelled "PROGRESS
REPORT".  The last one will say something like:

    =PROGRESS REPORT==== 16-Jan-2003::13:59:45 ===
             application: balance
              started_at: bal@davinci

Congratulations.  This means that the application is running
successfully.  If this isn't what you see, and you're certain that
you've followed all the directions, cut-and-paste the output and email
it to me.  (See footnote [2] below.)

The example configuration file, "../priv/be-list.config", is a proxy
for two SMTP servers, mx1.mail.yahoo.com and mx1.hotmail.com.  The
proxy is listening to local port 2525.

    % telnet davinci 2525
    Trying 10.10.10.10...
    Connected to localhost.localdomain.
    Escape character is '^]'.
    220 YSmtp mta614.mail.yahoo.com ESMTP service ready
    quit
    221 mta614.mail.yahoo.com
    Connection closed by foreign host.

See the "be-list.config" file for full details of the configuration.
To summarize:

    1. The proxy's local TCP port is 2525.
    2. The back-end connection timeout is 10 seconds.
    3. The back-end connection activity timeout is 2 minutes.
    4. The back-end host list:
       a. mx1.mail.yahoo.com, TCP port 25, 2 simultaneous sessions.
       b. bogus-demo, TCP port 25, 1 simultaneous sessions.
       c. mx1.hotmailcom, TCP port 25, 1 simultaneous sessions.

Use a Web browser to connect to the Web server running on TCP port
8080 on the machine running the balancer, e.g. http://davinci:8080/
and follow the link there.  You'll see something like (edited to fit
in 80 columns):

    Proxy start time: 2003/1/16 13:59:44
    Current time:     2003/1/16 15:39:21
    Local TCP port number: 2525
    Connection timeout (seconds): 10.0000
    Activity timeout (seconds): 120.000
    Length of wait list: 0
    
    Name               Port Status MaxConn ActConn ActiveCount ActiveTime
    mx1.mail.yahoo.com 25   up     2       0       1           2.94391
    bogus-demo         25   up     2       0       0           0
    mx1.hotmail.com    25   up     1       0       0           0

Now, do the following:

    1. Open four windows: xterm, terminal, Telnet application, or
       whatever.
    2. Use those windows to create four simultaneous TCP connections
       to the proxy.  E.g. "telnet davinci 2525".
    3. The first two clients should see greetings from a Yahoo mail
       exchanger.
    4. The third client should see a greeting from a HotMail mail
       exchanger.
    5. The fourth client should connect but otherwise see nothing.
    6. Type "QUIT" in the second client to terminate the session.
    7. The fourth client should then be connected to an available
       back-end host, namely a Yahoo server.
    8. In the second window, connect to the proxy again.
    9. Retrieve (or reload) the balancer's stats via its HTTP server.
       You should see that all three back-end sessions are busy, that
       there's one client in the "wait list", and that the status of
       the "bogus-demo" server has been changed to "down".


Changing the application's configuration on-the-fly
---------------------------------------------------

I haven't extended the balancer's HTTP server to be able to change the
balancer's config on-the-fly, but it's easy enough to do using
Erlang's native message passing mechanism.  It's clunkier than "click
here to change BE's status to 'down'", but hey, this was an
afternoon's hack!

Run "cd src" (if you aren't already there) and "erl -sname foo -pz
../ebin" on any machine that you've verified that the Erlang cookies
are correct & message passing works, then run the following Erlang
shell commands (changing the node name as appropriate):

    1. bal_proxy:get_state({balance, 'bal@davinci'}).	
    2. bal_proxy:reset_host({balance, 'bal@davinci'}, "mx1.mail.yahoo.com", down).
    3. bal_proxy:get_state({balance, 'bal@davinci'}).	

The first and third commands return raw state data maintained by the
balancing process: the HTTP server simply pretty-prints this data.
The second command sets the state of the "mx1.mail.yahoo.com" back-end
host to 'down'.  ('up' is the other valid state)

Other commands to experiment with are (you don't have to type them on
a single line, but you should, unless you're familiar with Erlang
syntax):

    bal_proxy:get_host({balance, 'bal@davinci'}, "mx1.hotmail.com").
    bal_proxy:reset_all({balance, 'bal@davinci'}).
    bal_proxy:del_be({balance, 'bal@davinci'}, "bogus-demo").
    bal_proxy:add_be({balance, 'bal@davinci'}, {be,"mx-ca-1.pobox.com",25,up,1,0,0,no_error,0,0,0,[]}, "").
    bal_proxy:add_be({balance, 'bal@davinci'}, {be,"smtp.TheWorld.com",25,up,1,0,0,no_error,0,0,0,[]}, "mx1.hotmail.com").

Use the bal_proxy:get_state() function to see how these functions
affect the state of the balancer.

NOTE: You probably shouldn't delete a back-end host unless you've
      marked its status as 'down' first ... and then waited for all
      active sessions to finish.  :-)

NOTE: The proxy has a bug (one of several, see comments in
      src/bal_proxy.erl) that happens if:
      1. All back-end hosts are status 'down'
      2. A proxy client connects.
      3. A back-end host is marked status 'up'
      Work-around: Don't allow this to happen.



Fault tolerance demonstrated by fault injection
-----------------------------------------------

One of the many applications distributed with Erlang is called
"appmon", the application monitor.  To start it, run "erl -sname
something" on any machine that you've verified that the Erlang cookies
are correct & message passing works, then run:

    appmon:start().

A GUI box should pop up that displays a tree of the applications
running on the local node.  In your case, "kernel" is probably the
only application running.

If you haven't already done so, run this command in the Erlang shell
(using the balancer's node name, of course):

    bal_proxy:get_state('bal@davinci').	

Then pull down the "Nodes" pull-down menu.  Both the local node and
'bal@davinci' should be listed.  Select the balancer's node.  You
should then see a tree of three applications running on the balancer:
kernel, sasl, and balance.  Click on the "balance" box.

Another window should appear.  This window displays the tree of
processes (Erlang threads, remember!), including "supervisor"
processes, used by the balancer application.

    * A top-level supervisor, named 'balance_sup'.
    * The 5 processes used by the "inets" HTTP server.
    * A variable number of processes used by the TCP balancer portion
      of the application: the socket listener process will always
      appear under the 'balance' process, as will the transient
      per-TCP-session processes.

Now, create a TCP connection to the balancer's port.  The tree will be
updated to show a second process underneath 'balance'.

A basic programming philosophy behind Erlang is "code only for the
common case".  If there's an error, e.g. divide by zero or an uncaught
exception, the default action is to kill the process.  You rely on
supervisor processes to restart abnormally-terminated processes.  Very
nice.

You can use "appmon" to send kill signals to any process it displays,
thus simulating a bug/software failure.  Do the following:

    1. Make a connection to the proxy's local port.
    2. Note which process appears under 'balance' when the appmon
       window is updated.  This is the process used to copy data back
       and forth between your client and the back-end host.
    3. Click on the "Kill" button.
    4. Click on the process box representing the process found in step
       number 2. 

This will terminate your client's TCP connection, as well as the
proxy's connection to the back-end host.

You can have more fun with this fault injection by:

    * Killing the socket accept()ing process underneath 'balance'.
    * Killing 'balance' or any of the HTTP server's processes.

If you do any of those things, the 'balance_sup' supervisor will kill
all remaining application processes and restart them.  It may happen
so quickly that the appmon window doesn't appear to change.  However,
look carefully at the process ID numbers of the socket listener
underneath 'balance' and the process underneath 'httpd_acc_sup_8080':
both will change, indicating that they aren't the same process that
used to be there.  That's the supervisor in action.

If you kill the 'balance_sup' process or any of its parents, the proxy
will crash and exit.  That's because 'balance_sup' doesn't have a
parent supervisor to restart it.  However, the *only* things that
supervisors do are:

    1. Start child processes.
    2. Monitor those children
    3. Restart any of those children, if they should be restarted.
    4. Exit if there are too many child failures within a configured
       amount of time.

The supervisor's code is assumed to be bug-free.  It probably is.  :-)

Typical Erlang application design uses a tree of supervisors.  If
there's a low level problem severe enough that the immediate
supervisor cannot deal with it, that supervisor will exit ... in
effect, passing the problem up the supervisor chain.  In the worst
case, the top-level supervisor can kill everything and restart from
scratch.  This kind of deterministic application startup and fault
handling is quite nice.

The 'balancer_sup' supervisor is configured to tolerate up to 5 child
deaths within a 30 second time period.  That probably isn't realistic
for real-world use, but it's fun for demonstration purposes.

For more detail on the process supervisor scheme of OTP, the Open
Telecom Platform, see http://www.erlang.org/doc/r9b/doc/system.html,
in particular the "Design Principles" document.


Hot code update
---------------

Like several other functional programming languages, Erlang permits
on-the-fly, hot code update.  The Erlang VM supports the notion of two
simultaneous loads of any particular module, current and new.  It's
way beyond the scope of this README to describe how this works or how
the Erlang OTP release handler can upgrade (or downgrade!) the running
code (and associated data structures) of one or more applications.
It's a nifty, if complex, feature.

This example will be much more basic: we'll add a line of output to
the HTTP server's status overview.  We would like the summary at the
top to include the balancer's Erlang node name.  That's easy enough to
do.  Follow these steps:

    1. Edit the file "src/bal_proxy.erl" with your favorite text
       editor.
    2. Locate the word "README", about 90% of the way from the top.
       You will insert code on the line immediately after this
       comment.  (The "%" character denotes the start of an Erlang
       comment; they continue to the end of the line.)
    3. Add the following line after the comment you found in step #2:

        io_lib:format("Proxy's Erlang node name: ~w\n", [node()]),

    4. Change working directory to "src", if you haven't already.
    5. Run "make", otherwise run
       "erlc -bbeam  +debug_info -o../ebin bal_proxy.erl"

Now that you've made the code change and recompiled it, we just need
to tell the balancer to load the new code.

First, just to make it really obvious what's going on, use your Web
browser to retrieve the balancer's current stats.

The balancer node was started using the "-noshell" command line flag,
so there is no Erlang shell available for us to modify the balancer's
internals.  So, we'll start a second node, then create a shell session
on the balancer's node, then use the second node's shell to
communicate with the balancer's node.

Run "cd src" (if you aren't already there) followed by "erl -sname
foo" on any machine that you've verified that the Erlang cookies are
correct & message passing works, then type the following things
(changing the node name as appropriate):

    1. Control-g
    2. r bal@davinci ENTER
    3. j ENTER
    4. c 3 ENTER

You should see something like this:

    % erl -sname foo
    Erlang (BEAM) emulator version 5.2 [source] [hipe] [threads:0]
    
    Eshell V5.2  (abort with ^G)
    (foo@rover)1> 
    User switch command
     --> r bal@davinci
     --> j
       1  {}
       2  {shell,start,[]}
       3* {bal@davinci,shell,start,[]}
     --> c 3
    Eshell V5.2  (abort with ^G)
    (bal@davinci)1>  

Any command you type in this command shell will be executed on the
'bal@davinci' node, *not* the local one.  Pretty slick, huh?

Type the command "l(bal_proxy)." into this shell, and you'll see:

    (bal@davinci)1> l(bal_proxy).
    {module,bal_proxy}
    (bal@davinci)2> 

Now, tell your Web browser to reload the stats page.  Notice that your
new code has indeed been executed!


Using tcpbalance with distcc
----------------------------

See the file "priv/sample-distcc.config" for an example config for 6
back-end machines with different numbers of CPUs and different CPU
speeds.


Questions, bugs, etc.
---------------------

If you have questions, bug reports, etc., please email them to me.  My
email address is in footnote [2] below.  Tcpbalance isn't meant to be
a 100% bulletproof, full-featured distcc application proxy ... but
that doesn't mean that I'm not willing to help out or perhaps fix
bugs.

Martin Pool, distcc's maintainer, suggested that this Erlang proxy
could be a model for a "real" bulletproof, full-featured distcc
application proxy that someone might write someday.  I think that is a
*great* idea!  In the meantime, I'll continue using this proxy....

-Scott Lystig Fritchie



Footnotes
---------

[1]  If you really want to use a machine with a hyphen in its DNS
hostname, you can do it.  You just need to put single quotes around
the node name whenever you use it.  For example, if you run "erl
-sname foo" on a machine called "nt-regal", then whenever this
document asks you to type a node name into the Erlang shell, you must
type:
	'foo@nt-regal'
... instead of:
	foo@nt-regal
The latter is incorrect Erlang syntax.

Technically, a node name is treated as an Erlang atom, a primitive
data type, much like an atom in Lisp or Scheme.  An atom typically
starts with a lowercase letter and may be alphanumeric or underscore
("_").  However, if you want an atom to contain other characters or to
start with an upper-case letter, it can be surrounded by single
quotes.  For example,
	'This_is_an_atom'
is valid syntax for an atom.

[2] My Internet email address: the lefthand side of the "@" is "slf".
The righthand side is "caspiannetworks.com".
