#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import smtplib
from email.message import EmailMessage
import random
import argparse
import re
import getpass

parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter, description="""\
A hopefully handy script that randomly compute Kris Kindle assignments
and sends out e-mails letting each person involved know who they have
to buy a gift for.

Note: you need an SMTP relay account to run this script. GMail used to
provide a nice SMTP relay service this but no longer does so for free,
so you might want to consider alternatives (such as SendInBlue).
""")

parser.add_argument("-f", "--people-file", dest="people_file", required=True, help="""\
File describing the Kris Kindle participants. The format of the file
is as follows:
  {name}, {email address} [,{optional comma separated excluded names}]
So for example if Anna is participating but shouldn't be assigned Kiera
and vice versa, the Anna and Kiera lines in the file should written:
Anna, anna@example.com, Kiera
Kiera, kiera@example.com, Anna""")

parser.add_argument("-n", "--no-send", dest="no_send", action='store_true', help="""\
Dont send emails; instead show what would have been sent""")

parser.add_argument("--only-email", dest="only_email", help="""\
If mails are to be sent, they will only be sent to this e-mail address.
Useful for testing to see how the received e-mails look.""")

parser.add_argument("--from", dest="from_user", help="""\
If mails are to be sent, the sender is this e-mail address. Note that this email
address should be the same as the address associated with SMTP relay account or
otherwise the generated mail will look like spam. Defaults to the user name of
the SMTP relay account.""")

parser.add_argument("-t", dest="send_test_emails", action='store_true', help="""\
Send test e-mails to participants. The test e-mail does not contain the
Kris Kindle assignments and it's useful for validating that the email
addresses that you have are correct.""")

parser.add_argument("-u", "--user", dest="user", help="""\
Username associated with SMTP relay account that will be used to send the e-mails.""")

parser.add_argument("-p", "--password", dest="password", help="""\
Password associated with SMTP relay account username; will be prompted if it's
not supplied on the command line""")

args = parser.parse_args()
if not args.password:
    args.password = getpass.getpass()

if not args.from_user:
    args.from_user = args.user

SUBJECT = "Your Kris Kindle 2024 assignment!"
MESSAGE = """\
Hello {person}!

Your assignment in the 2024 Kris Kindle is: <b>{assignment}</b>.

Can you believe it's November already? Where does the time go.

You might be interested to learn that there's a new rule added to the Kris Kindle logic this year: you can't get \
someone that you got last year. (Don't worry, Ailbhe, I got you covered!)

The suggested amount to spend is <b>approximately €30 or $30</b>. No problem if you'd to spend a little more or less.

Here's a couple of Christmas jokes to get you in a yuletide mood:

Q: Why are Santa's helpers depressed?
A: <s>The election.</s> Because they have low elf-esteem.

Q: What do you call enthusiasts of the game of chess bragging about past wins in a hotel lobby?
A: Chess nuts boasting in an open foyer.

Let me know if you have any questions. Have fun shopping for your Kris Kindle!

Kris :-)"""

TEST_SUBJECT = "Test email from Kris"
TEST_MESSAGE = """Hello {person}
This is a test e-mail from your friend Kris, to see if I have your email address correct.
No action is necessary on your part and you can feel free to delete this mail.
Yours,
Kris :-)"""


class Person:
    def __init__(self, name, email, exclusions):
        self.name = name
        self.email = email
        self.exclusions = exclusions

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.__str__()

    @staticmethod
    def read_people(person_file):
        with open(person_file) as f:
            result = []
            person_map = {}
            for line in f:
                line = line.strip()
                if line.startswith('#'):
                    continue
                values = re.split(r'\s*,\s*', line)
                exclusions = []
                if len(values) > 2:
                    exclusions = values[2:]
                elif len(values) < 2:
                    print("Invalid person data: ", line)
                    exit(1)
                p = Person(values[0], values[1], exclusions)
                if p.name in person_map:
                    print("Duplicate name: ", line)
                    exit(2)
                result.append(p)
                person_map[p.name] = p
            for p in result:
                for excluded in p.exclusions:
                    if excluded not in person_map:
                        print("Excluded name not recognized for: {0} excluded name: '{1}'".format(p, excluded))
                        exit(3)
            return result


def send_mail(from_addr, to_addr, to_name, subject, message):
    msg = EmailMessage()
    msg.set_content(message)
    msg["Subject"] = subject
    msg["From"] = f"Kris-Kindle <{from_addr}"
    msg["To"] = f"{to_name} <{to_addr}>"

    if args.no_send:
        print("----------------------------------------------------")
        print("would send this message to: {0}\n{1}".format(to_addr, message))
        return

    if not args.only_email or args.only_email == to_addr:
        print("sending message to {0}".format(to_addr))

        # s = smtplib.SMTP('smtp.sendgrid.net:587')
        s = smtplib.SMTP('smtp-relay.sendinblue.com:587')

        s.ehlo()
        s.starttls()
        s.login(args.user, args.password)
        s.send_message(msg)
        s.quit()


def maybe_assign(people):
    """
    :param people: list of Persons to assign
    :return: dict mapping assignee to person who has the assignee or an empty
             dictionary if the attempt to assign didn't work
    """
    people = sorted(people, key=lambda x: len(x.exclusions), reverse=True)
    print(people)
    assignments = {}
    for person in people:
        recipients = [p for p in people
                      if (p.name not in assignments and
                          p != person and
                          p.name not in person.exclusions)]
        if not recipients:
            print("didn't work")
            print("assigned: {0}".format(assignments))
            print("{0} -> {1}".format(person, recipients))
            return {}
        recipient = recipients[random.randint(0, len(recipients) - 1)]
        assignments[recipient.name] = person.name
    return assignments


def main():
    people = Person.read_people(args.people_file)

    if args.send_test_emails:
        for p in people:
            message = TEST_MESSAGE.format(person=p.name)
            send_mail(args.from_user, p.email, p.name, TEST_SUBJECT, message)
        exit(0)

    assignments = {}
    for attempt in range(10):
        assignments = maybe_assign(people)
        if assignments:
            break

    people_map = {p.name: p for p in people}
    if assignments:
        print("success!")
        for p in assignments:
            print("{0}\t->\t{1}".format(assignments[p], p))
        valid = True
        for recipient in assignments:
            buyer = people_map[assignments[recipient]]
            if recipient in buyer.exclusions:
                print("broken!")
                valid = False
        if valid:
            for p in assignments:
                message = MESSAGE.format(person=assignments[p], assignment=p)
                to_person = people_map[assignments[p]]
                send_mail(args.from_user, to_person.email, to_person.name,
                          SUBJECT, message)
    else:
        print("didn't work - you will need to rerun")
        exit(1)

if __name__ == "__main__":
    main()
