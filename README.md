# The Steak Place
Doug Orleans's personal website. Currently runs on [Racket Web Server](http://docs.racket-lang.org/web-server/),
but in the process of being converted to Ruby on Rails.

Steps to install (from this directory):

0. You may need to edit some config files if this directory is not in /home/dougo/steak-place.
1. sudo apt-get install nginx racket
2. sudo ln -s /home/dougo/steak-place/plt-web-server /etc/init.d
3. sudo update-rc.d plt-web-server defaults
4. sudo service plt-web-server start
5. sudo ln -s /home/dougo/steak-place/nginx /etc/nginx/sites-available/steak-place
6. sudo ln -s ../sites-available/steak-place /etc/nginx/sites-enabled
7. sudo rm /etc/nginx/sites-enabled/default
8. sudo service nginx restart

