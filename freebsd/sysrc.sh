#!/bin/sh

sysrc sendmail_enable=NO
sysrc rc_startmsgs=NO
sysrc rc_info=NO
sysrc rc_debug=NO
sysrc sshd_enable=YES
sysrc ntpd_enable=YES
sysrc log_in_vain=YES
sysrc local_unbound_enable=YES
sysrc -f /boot/loader.conf fusefs_load=YES
