FROM debian:bookworm-slim
MAINTAINER someodd, https://github.com/someodd

RUN apt-get update
RUN apt-get install -y git
RUN useradd spacecookie
ADD spacecookie.json /etc/spacecookie.json
RUN mkdir -p /srv/gopher

# FIXME: if i don't do this hgetfilecontents or whatever will fail
RUN apt-get install -y locales
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN locale-gen

# The git server
RUN adduser --disabled-password --gecos "" git
RUN mkdir -p /srv/git
RUN chown -R git:git /home/git
RUN chown -R git:git /srv/git
RUN chown -R git:git /srv/gopher
RUN chown -R git:git /srv
ADD id_rsa.pub /tmp/id_rsa.pub
RUN chown -R git:git /tmp/id_rsa.pub
USER git
RUN cd /home/git
RUN mkdir /home/git/foo
RUN mkdir /home/git/.ssh
RUN chmod 700 /home/git/.ssh
RUN touch /home/git/.ssh/authorized_keys && chmod 600 /home/git/.ssh/authorized_keys
# Needs to mount /tmp/id_rsa.pub?
RUN echo "no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty $(cat /tmp/id_rsa.pub)" >> /home/git/.ssh/authorized_keys
# initialize the repo
RUN cd /srv/git
RUN mkdir /srv/git/gopherhole.git
RUN cd /srv/git/gopherhole.git
RUN git init --bare /srv/git/gopherhole.git
ADD post-receive /srv/git/gopherhole.git/hooks/
#RUN chmod +x /srv/git/gopherhole.git/hooks/post-receive
USER root
#RUN adduser gopher
RUN chsh -s /usr/bin/git-shell git

# now add a server-side git hook for burrow
RUN apt-get install -y wget
ADD latest-deb.sh /tmp/latest-deb.sh
RUN chmod +x /tmp/latest-deb.sh
RUN /tmp/latest-deb.sh
RUN apt-get install /tmp/burrow.deb

RUN apt-get install -y openssh-server
RUN service ssh start

# Add the update script
RUN apt-get install -y cron
ADD auto_update.sh /usr/local/bin/auto_update.sh
RUN chmod +x /usr/local/bin/auto_update.sh

# Add crontab entry if the flag is set
ARG SYNC_MODE=false
RUN if [ "$SYNC_MODE" = "true" ]; then \
    echo "0 * * * * /usr/local/bin/auto_update.sh" > /etc/cron.d/check_updates; \
    chmod 0644 /etc/cron.d/check_updates; \
    crontab /etc/cron.d/check_updates; \
    service cron start; \
fi

EXPOSE 7071
EXPOSE 2222

ADD ./launch.sh /launch.sh
RUN chmod +x /launch.sh
ENTRYPOINT ["/launch.sh"]
