#!/usr/bin/env python
import smtplib
import random
import argparse
import re
import getpass

parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter, description="""\
A hopefully handy script that randomly compute Kris Kindle assignments
and sends out e-mails letting each person involved know who they have
to buy a gift for.

Note: currently you need an Gmail address to run this script and
additionally you will need to configure your Gmail account to allow
'low security' apps to connect to it and use it as an SMTP server.
Once you're logged in to the appropriate GMail account, you can do
this from the https://www.google.com/settings/security/lesssecureapps
page.

Don't worry: this all doesn't mean your Gmail password will be sent in
the clear. The script sets up a TLS connection to the server and
login and the email themselves are encrypted and the 'low security apps'
setting can be reverted after you send the e-mails. """)
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
parser.add_argument("-t", dest="send_test_emails", action='store_true', help="""\
Send test e-mails to participants. The test e-mail does not contain the
Kris Kindle assignments and it's useful for validating that the email
addresses that you have are correct.""")
parser.add_argument("-u", "--user", dest="user", required=True,
                    help="Gmail account that will be used to send the e-mails", )
parser.add_argument("-p", "--password", dest="password",
                    help="Password; you will be prompted if it's not supplied on the command line")
args = parser.parse_args()
if not args.password:
    args.password = getpass.getpass()


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
                values = re.split('\s*,\s*', line)
                exclusions = []
                if len(values) > 2:
                    exclusions = values[2:]
                elif len(values) < 2:
                    print "Invalid person data: ", line
                    exit(1)
                p = Person(values[0], values[1], exclusions)
                if p.name in person_map:
                    print "Duplicate name: ", line
                    exit(2)
                result.append(p)
                person_map[p.name] = p
            for p in result:
                for excluded in p.exclusions:
                    if excluded not in person_map:
                        print "Excluded name not recognized for: {0} excluded name: '{1}'".format(p, excluded)
                        exit(3)
            return result


def sendmail(to_addr, subject, message):
    from_addr = args.user
    full_msg = [
        "From: {0}".format(from_addr),
        "To: {0}".format(to_addr),
        "Subject: {0}".format(subject),
        ""
    ] + message
    msg = "\r\n".join(full_msg)

    if args.no_send:
        print "would send this message to: {0}\n{1}".format(to_addr, message)
        return

    if args.only_email and args.only_email != to_addr:
        return
    print "sending message to {0}".format(to_addr)

    s = smtplib.SMTP('smtp.gmail.com:587')
    s.ehlo()
    s.starttls()
    s.login(args.user, args.password)
    s.sendmail(from_addr, to_addr, msg)
    s.quit()


def maybe_assign(people):
    people = sorted(people, key=lambda x: len(x.exclusions), reverse=True)
    print people
    assignments = {}
    for person in people:
        recipients = [p for p in people
                      if (p.name not in assignments and
                          p != person and
                          p.name not in person.exclusions)]
        if not recipients:
            print "didn't work"
            print "assigned: {0}".format(assignments)
            print "{0} -> {1}".format(person, recipients)
            return {}
        recipient = recipients[random.randint(0, len(recipients) - 1)]
        assignments[recipient.name] = person.name
    return assignments


def main():
    people = Person.read_people(args.people_file)

    if args.send_test_emails:
        for p in people:
            message = [
                "Hello {0}:".format(p.name),
                "This is a test e-mail to see if I have the email addresses correct and to "
                "help me debug any email sending issues I have. No action is necessary on your part "
                "and you can feel free to delete this mail.",
                "Yours,",
                "Kris"
            ]
            sendmail(p.email, "Test email from Kris", message)
        exit(0)

    assignments = {}
    for attempt in range(5):
        assignments = maybe_assign(people)
        if assignments:
            break

    people_map = {p.name: p for p in people}
    if assignments:
        print "success!"
        for p in assignments:
            print "{0}\t->\t{1}".format(assignments[p], p)
        valid = True
        for recipient in assignments:
            buyer = people_map[assignments[recipient]]
            if recipient in buyer.exclusions:
                print "broken!"
                valid = False
        if valid:
            for p in assignments:
                message = [
                    "Hello {0}:".format(assignments[p]),
                    "",
                    "Welcome to the 2015 incarnation of our long-established tradition! If you believe "
                    "there to be a problem with your assignment let me know and we can rerun.",
                    "",
                    "Your present-buying assignment is: {0}".format(p),
                    "",
                    "Happy Christmas-shopping!",
                    "Kris :-)"
                ]
                to_person = people_map[assignments[p]]
                sendmail(to_person.email, "Your Kris Kindle 2015 assignment!", message)
    else:
        print "didn't work - you will need to rerun"
        exit(1)

if __name__ == "__main__":
    main()
