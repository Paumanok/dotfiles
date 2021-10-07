#!/bin/sh
# shell script to prepend i3status with cmus song and artist

i3status | while :
do
        read line
        artist=$(cmus-remote -Q | grep ' artist ' | cut -d ' ' -f3-)
        song=$(cmus-remote -Q | grep title | cut -d ' ' -f3-)
        echo "$artist - $song                                                       $line" || exit 1
done

# Start i3bar to display a workspace bar (plus the system information i3status
# finds out, if available)
#bar {
#        #status_command i3status
#        status_command ~/.statusbar/mystatus.sh #new status command
#        mode dock
#        position top
#}

#If for some reason you want to change back, just uncomment the first line after bar and comment the second one.
#do it right later http://code.stapelberg.de/git/i3status/tree/contrib/wrapper.py
