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


class GmailNotifier(object):
  def __init__(self, poll=60):
    self.poll_interval = poll
    self.user = "abdo.roig"

    self.debug = 1

    self.conn = imaplib.IMAP4_SSL(host = 'imap.gmail.com', port = imaplib.IMAP4_SSL_PORT)
    self.unread_stuff = set([])

    self.lastmail_path = os.path.join(os.getenv('XDG_RUNTIME_DIR'), 'lastmail')


  def login(self):
    netrc_file = os.path.expanduser('~/.netrc')
    if os.path.exists(netrc_file):
      try:
        auth = netrc.netrc(netrc_file).authenticators('google.com')
      except netrc.NetrcParseError as err:
        print("Google credentials not configured. Need user and password")
        sys.exit(-1)
      if auth:
        return self.conn.login(auth[0], auth[2])
      else:
        print("Google credentials not configured. Need user and password")
        sys.exit(-1)


  def load_unreadstuff(self):
    if os.path.exists(self.lastmail_path):
      with open(self.lastmail_path, 'r') as fd:
        return set([int(uid.strip()) for uid in fd.read().split('\n') if len(uid.strip()) > 0])
    else:
      return set([])


  def save_unreadstuff(self, unreadstuff):
    with open(self.lastmail_path, 'w') as fd:
      for uid in unreadstuff:
        fd.write("%d\n" % uid)


  def decode_subject(self, subj):
    ret=""
    for t,c in decode_header(subj):
      if c:     ret = ret + t.decode(c)
      else:     ret = ret + t.decode()
    return ret


  def decode_utf8(self, text):
    try:
      text_dec = unicode(text, 'utf-8')
      return text_dec
    except UnicodeDecodeError:
      return text


  def parse_response(self, data):
    p = Parser()
    try:
      uid = int(re.search("UID\s*([0-9]*)\s*BODY", data[0][0].decode('utf-8')).group(1))
      raw_msg = p.parsestr(data[0][1].decode('utf-8'))
    except:
      return (None, None)

    msg = {}
    msg['from'] = re.search("[^ ,<]*@[^ ,>]*", raw_msg['from']).group(0)
    msg['subject'] = raw_msg['subject']
    return (uid,msg)


  def desktop_notification(self, title, msg, urgency='normal'):
    subprocess.call(['notify-send', '-a', 'gnoti', '-u', urgency, title, msg])


  def desktop_mail_notification(self, msg):
    line = "%s: %s" % (msg['from'], msg['subject'])
    print("Notification: %s" % line)
    noti = "%s\n%s" % (msg['from'], msg['subject'])
    self.desktop_notification("New Mail", noti)
    # subprocess.call(['palantir -o mail -p 5 "%s"' % line], shell=True)


  def sync_mail(self):
    try:
      subprocess.call(['systemctl', '--user', '--no-block', 'start', 'fetch-mail.service'])
      for k, msg in new_messages.items():
        self.desktop_mail_notification(msg)

      return True
    except Exception:
      return False


  def notify_error(self):
    self.desktop_notification("Error", "Error in gnoti. Stopping", urgency='critical')


  def unread_locally(self):
    raw = subprocess.check_output(['mutag -p mail -C -q "flag:unread AND tag:\\\\Inbox"'], shell=True)
    return int(raw)


  def new_mail(self):
    self.conn.select('Inbox', readonly=True)
    ret, raw_messages = self.conn.search(None, '(UNSEEN)')

    messages = {}
    new_messages = {}
    if ret == 'OK':
      for m in [uid.decode('utf-8') for uid in raw_messages]:
        if m == '': continue
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
    # Enter idle state
    still_idle=True
    synced = False
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


  def loop(self):
    while True:
      try:
        (ret, capabilities) = self.login()
      except:
        print(sys.exc_info()[1])
        sys.exit(1)

      while ret == 'OK':
        print("Checking for new mail")
        new_mail = self.new_mail()
        if len(new_mail) > 0:
          self.sync_mail()
          for k, msg in new_mail.items():
            self.mail_notification(msg)

        print("Sleep")
        time.sleep(self.poll_interval)
        # Did not work ok
        # idle_loop(self.unread_stuff)

    self.conn.close()

# one time check and sync
gn = GmailNotifier()
gn.login()

new_mail = gn.new_mail()
if len(new_mail) == 1:  print("%d new message found" % len(new_mail))
elif len(new_mail) > 1: print("%d new messages found" % len(new_mail))

if len(new_mail) > 0:
  gn.sync_mail()
  for k, msg in new_mail.items():
    gn.desktop_mail_notification(msg)
