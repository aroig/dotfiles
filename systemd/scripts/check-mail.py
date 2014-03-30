#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import time
import netrc
import subprocess
import imaplib
# import imaplib2 as imaplib        # need imaplib2 for the idle feature

from email.parser import Parser
from email.header import decode_header

from optparse import OptionParser


class GmailNotifier(object):
    """Checks for new messages in gmail and triggers a systemd unit when new messages appear."""


    def __init__(self, poll=60, user=None, debug=False):
        self.poll_interval = poll
        self.user = user
        self.debug = debug

        self.conn = None
        self.unread_stuff = set([])

        self.lastmail_path = os.path.join(os.getenv('XDG_RUNTIME_DIR'), 'lastmail')



    def login(self):
        """Login to gmail's IMAP"""

        netrc_file = os.path.expanduser('~/.netrc')
        host = 'imap.gmail.com'

        # check if there is a connection alive
        if self.conn:
            try:
                self.conn.noop()
            except:
                self.logout()

        if self.conn == None:
            try:
                self.conn = imaplib.IMAP4_SSL(host = host, port = imaplib.IMAP4_SSL_PORT)

            except:
                print("can't establish a connection to %s" % host)
                raise

            if os.path.exists(netrc_file):
                try:
                    auth = netrc.netrc(netrc_file).authenticators('google.com')
                    self.conn.login(auth[0], auth[2])

                except netrc.NetrcParseError as err:
                    print("google credentials not configured. Need user and password")
                    raise



    def logout(self):
        """Logout from gmail"""
        if self.conn:
            self.conn.close()
            self.conn.logout()
            self.conn = None



    def load_unreadstuff(self):
        """Returns a set of uid's for unread messages"""
        if os.path.exists(self.lastmail_path):
            with open(self.lastmail_path, 'r') as fd:
                return set([int(uid.strip()) for uid in fd.read().split('\n') if len(uid.strip()) > 0])
        else:
            return set([])



    def save_unreadstuff(self, unreadstuff):
        """Writes uid's into a file, under $XDG_RUNTIME_DIR"""
        with open(self.lastmail_path, 'w') as fd:
            for uid in unreadstuff:
                fd.write("%d\n" % uid)



    def decode_header(self, raw):
        """Decodes a raw header and returns it as a string"""
        ret = ""
        for txt, enc in decode_header(raw):
            if   enc == 'unknown':      ret = ret + str(txt, 'ascii', errors='ignore')
            elif enc == 'unknown-8bit': ret = ret + str(txt, 'utf-8', errors='ignore')
            elif enc != None:           ret = ret + str(txt, enc, errors='ignore')
            elif type(txt) != str:      ret = ret + str(txt, 'utf-8', errors='ignore')
            else:                       ret = ret + txt

        return ret



    def parse_response(self, data):
        """Parses an IMAP response and returns a tuple with uid and message."""
        p = Parser()
        try:
            uid = int(re.search("UID\s*([0-9]*)\s*BODY", data[0][0].decode('utf-8')).group(1))
            raw_msg = p.parsestr(data[0][1].decode('utf-8'))
        except:
            return (None, None)

        msg = {}
        # msg['from'] = re.search("[^ ,<]*@[^ ,>]*", raw_msg['from']).group(0)
        msg['from'] = self.decode_header(raw_msg['from'])
        msg['subject'] = self.decode_header(raw_msg['subject'])
        return (uid,msg)



    def desktop_notification(self, title, msg, urgency='normal'):
        """Produces a desktop notification via notify-send"""
        subprocess.call(['notify-send', '-a', 'mail',
                         '-u', urgency, title, msg])



    def notify_new_mail(self, msg):
        """Produces a desktop notification for new mail."""
        line = "%s: %s" % (msg['from'], msg['subject'])
        noti = "%s\n%s" % (msg['from'], msg['subject'])
        self.desktop_notification("New Mail", noti)



    def notify_error(self):
        """Produces a desktop notification for an error."""
        self.desktop_notification("Error", "Error in check-mail. Stopping", urgency='critical')



    def sync_mail(self):
        """Triggers the fetch-mail.target for systemd --user, that fetches and indexes new mail"""
        try:
            subprocess.call(['systemctl', '--user', 'start', 'fetch-mail.target'])
            for k, msg in new_messages.items():
                self.notify_new_mail(msg)

            return True
        except Exception:
            return False



    def unread_locally(self):
        """Get the number of unread messages in the local database"""

        raw = subprocess.check_output(['mutag -p mail -C -q "flag:unread AND tag:\\\\Inbox"'], shell=True)
        return int(raw)



    def new_mail(self):
        """Get a list of new message uid's since last check"""

        self.conn.select('Inbox', readonly=True)
        ret, raw_messages = self.conn.search(None, '(UNSEEN)')
        raw_uids = ' '.join([rm.decode('utf-8') for rm in raw_messages]).split(' ')

        messages = {}
        new_messages = {}
        if ret == 'OK':
            for m in [uid.strip() for uid in raw_uids if len(uid.strip()) > 0]:
                ret, mesginfo = self.conn.fetch(m, '(UID BODY.PEEK[HEADER])')
                if ret == 'OK':
                    uid, msg = self.parse_response(mesginfo)
                    if uid:
                        messages[uid] = msg

        if len(messages) > 0:
            self.unread_stuff = self.load_unreadstuff()
            new_messages = {d: messages[d] for d in messages if d not in self.unread_stuff}
            self.unread_stuff = set(messages.keys())
            self.save_unreadstuff(self.unread_stuff)

        return new_messages



    def idle_loop(self, unread_stuff):
        """Loop using IDLE IMAP feature to get new mail as soon as it arrives"""
        # Enter idle state
        still_idle=True
        synced = False
        self.login()

        while still_idle:
            if unread_locally() == 0 and len(unread_stuff) > 0:
                if not synced:
                    sync_mail()
                    synced = True
                else:
                    notify_error()
                    raise Exception

                still_idle = False
            else:
                synced = False

            T1 = time.time()
            if self.debug: print("Idle (%d)" % len(unread_stuff))
            (ret, data) = self.conn.idle(timeout=10)
            if self.conn.response('IDLE')[1] != ['TIMEOUT']:
                still_idle = False
            T2 = time.time()

            #If been idle for a short time, take it easy.
            if (T2 - T1) < 2:
                time.sleep(5)

        self.logout()



    def _check(self, quiet=False, notify=True):
        """Check for new mail once. Do not disconnect"""

        try:
            self.login()

            if self.debug: print("checking for new mail")
            new_mail = self.new_mail()

            if not quiet:
                if len(new_mail) == 1:  print("%d new message found" % len(new_mail))
                elif len(new_mail) > 1: print("%d new messages found" % len(new_mail))

            if len(new_mail) > 0:
                self.sync_mail()

            if notify:
                for k, msg in new_mail.items():
                    self.notify_new_mail(msg)

        except:
            print("an error occurred checking mail")
            if self.debug: raise



    def loop(self, quiet=False, notify=True):
        """Loop that polls gmail regularly for new messages"""

        while True:
            self._check(quiet=quiet, notify=notify)

            if self.debug: print("sleep %d" % self.poll_interval)
            time.sleep(self.poll_interval)
            # Did not work ok
            # idle_loop(self.unread_stuff)

        self.logout()



    def check(self, quiet=False, notify=True):
        """Check for new mail once and disconnect."""

        self._check(quiet=quiet, notify=notify)
        self.logout()



usage = """usage: %prog [options]

Checks for new mail in gmail. Uses the user and password configured in .netrc for gmail's IMAP server.
"""

parser = OptionParser(usage=usage)

parser.add_option("-u", "--user", action="store", type="string", default=None, dest="user",
                  help="Username to login to gmail's IMAP. The password is obtained from .netrc")

parser.add_option("-w", "--wait", action="store", type="int", default=5, dest="wait",
                  help="Time to wait between mail checks in minutes. Default: 5.")

parser.add_option("-d", "--daemon", action="store_true", default=False, dest="daemon",
                  help="Run as a daemon, checking mail periodically.")

parser.add_option("-q", "--quiet", action="store_true", default=False, dest="quiet",
                  help="Do not print status messages on stdout.")

parser.add_option("-n", "--notify", action="store_true", default=False, dest="notify",
                  help="Emmit desktop notifications for new mail.")

parser.add_option("--debug", action="store_true", default=False, dest="debug",
                  help="Print debug messages.")

(opts, args) = parser.parse_args()

gn = GmailNotifier(user=opts.user,
                   poll=60*opts.wait,
                   debug=opts.debug)


try:
    if not opts.daemon:
        gn.check(quiet=opts.quiet, notify=opts.notify)

    else:
        gn.loop(quiet=opts.quiet, notify=opts.notify)


except KeyboardInterrupt:
    print("")
    sys.exit(0)
