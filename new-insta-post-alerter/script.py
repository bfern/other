import instaloader
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import argparse
import yaml
import time
import os

def load_config(config_file):
    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)
    return config['gmail']['address'], config['gmail']['password']

def check_new_posts(username):
    L = instaloader.Instaloader()
    profile = instaloader.Profile.from_username(L.context, username)

    # Get the latest post
    latest_post = None
    for post in profile.get_posts():
        latest_post = post
        break

    return latest_post

def send_email(username, password, recipient_email, subject, message):
    # Set up the email server
    server = smtplib.SMTP(host='smtp.gmail.com', port=587)
    server.starttls()
    server.login(username, password)

    # Create message
    msg = MIMEMultipart()
    msg['From'] = username
    msg['To'] = recipient_email
    msg['Subject'] = subject
    msg.attach(MIMEText(message, 'plain'))

    # Send email
    server.send_message(msg)
    server.quit()

def main():
    # Parse command-line arguments
    parser = argparse.ArgumentParser(description='Send email notification for new Instagram posts.')
    parser.add_argument('recipient_email', type=str, help='Recipient email address')
    args = parser.parse_args()
    recipient_email = args.recipient_email

    # Load email credentials from config file
    home_dir = os.path.expanduser("~")
    config_file = os.path.join(home_dir, '.config', 'config.yaml')
    email_username, email_password = load_config(config_file)

    username = "pickybits"

    latest_post = check_new_posts(username)
    previous_latest_post = None
    try_to_send_email = True
    while try_to_send_email:
        
        if (previous_latest_post is not None) and (previous_latest_post != latest_post):

        # Check if the latest post is different from the previous one
        # You can store the latest post in a file or database to compare
        # For simplicity, I'm just printing the link to the latest post
            print("New post from {}: {}".format(username, latest_post.url))
            
            # Send email notification
            send_email(email_username, email_password, recipient_email, "New post from {}".format(username), "Picky Bits has a new post on instagram, it could be their menu drop! This is an auto generated email from Brad's Python code. The following should be a link to the new post: " + latest_post.url)
            
            # Wait for some time before checking again (e.g., every hour)
            try_to_send_email = False
        
        previous_latest_post = latest_post
        print("Sleeping for an hour.")
        time.sleep(3600)  # 3600 seconds = 1 hour

if __name__ == "__main__":
    main()
