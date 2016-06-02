# The Steak Place
Doug Orleans's personal website. Currently runs on [Racket Web Server](http://docs.racket-lang.org/web-server/),
but in the process of being converted to Ruby on Rails.

Steps to install on a new server (from this directory):

1. You may need to edit some config files if this directory is not in /home/dougo/steak-place.
2. sudo apt-get install libssl-dev libreadline-dev libsqlite3-dev nodejs racket
3. Install rbenv and rbenv-install: https://github.com/rbenv/rbenv#installation
4. rbenv install 2.3.0 (or something later)
5. Install rbenv-vars: git clone https://github.com/rbenv/rbenv-vars.git ~/.rbenv/plugins/rbenv-vars
6. Install Phusion Passenger: https://www.phusionpassenger.com/library/walkthroughs/deploy/ruby/ownserver/nginx/oss/trusty/install_passenger.html
7. sudo ln -s /home/dougo/steak-place/plt-web-server /etc/init.d
8. sudo update-rc.d plt-web-server defaults
9. sudo service plt-web-server start
10. sudo ln -s /home/dougo/steak-place/nginx /etc/nginx/sites-available/steak-place
11. sudo ln -s ../sites-available/steak-place /etc/nginx/sites-enabled
12. sudo rm /etc/nginx/sites-enabled/default
13. sudo service nginx restart
14. bundle
15. echo "SECRET_KEY_BASE=`rake secret`" > .rbenv-vars

Steps to deploy a new release:
1. git pull
2. bundle
3. rake assets:precompile
4. passenger-config restart-app $PWD
