The spam classifier is based on a trained SVM model and the [Spam Assassin](https://spamassassin.apache.org) public mail corpus. The Spam Assassin corpus is a selection of mail messages splitted in differents categories: spams , hams that are real emails and hard hams representing real email which can be mistaken for spams. The corpus contains 6047 messages with about a 31% spam emails.

The the developped SVM model gives an accuracy of 96% on the email corpus and about 85% accuracy on the hard ham category.
