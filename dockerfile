FROM rocker/tidyverse:4.0.5
ENV USER="rstudio"
COPY --chown=rstudio ./switch-to-project /home/rstudio/.local/share/rstudio/projects_settings/switch-to-project
COPY --chown=rstudio . /home/rstudio/PCI_analysis
CMD ["/usr/lib/rstudio-server/bin/rserver","--server-daemonize=0","--auth-none=1"]

