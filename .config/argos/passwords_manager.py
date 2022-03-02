#!/usr/bin/env python3

import re
from configparser import RawConfigParser
from gi.repository import Gio
from os.path import expanduser

websites = {}

config_directory = expanduser("~") + '/.config/passwords-manager/'
accounts_file = config_directory + 'accounts.ini'
config_file = config_directory + 'config.ini'

config = RawConfigParser()
config.read(accounts_file)

for website in config.sections():
    websites[website] = []

    accounts = config.options(website)
    for account in accounts:
        websites[website].append(account)

print("| iconName=dialog-password-symbolic")
print("---")
for website, accounts in sorted(websites.items()):
  print(website)
  for account in sorted(accounts):
    print("--%s | useMarkup=false bash='python ~/workspace/python/password-manager/main-bash.py %s' terminal=false" % (config.get(website, account), website + '@' + config.get(website, account)))
    # print("--%s | useMarkup=false bash='python ~/workspace/python/password-manager/main-bash.py %s; touch ~/.config/argos/password_manager.py' terminal=false" % (config.get(website, account), website + '@' + config.get(website, account)))