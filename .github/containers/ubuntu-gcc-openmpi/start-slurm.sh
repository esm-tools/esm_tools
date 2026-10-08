#!/bin/bash
#
# Start a single-node Slurm inside the container, sized to what the container
# sees. esm_runscripts needs a batch system; this gives it one.
#
# Runs as the normal user and uses sudo for the daemons.

set -e

HOST=$(hostname -s)
CPUS=$(nproc)
MEM_MB=$(awk '/MemTotal/ {print int($2 / 1024)}' /proc/meminfo)

sudo tee /etc/slurm/slurm.conf > /dev/null <<EOF
ClusterName=awiesm3
SlurmctldHost=${HOST}
SlurmUser=slurm
AuthType=auth/munge
# No cgroups or task binding: Docker does not hand them to the container.
ProctrackType=proctrack/linuxproc
TaskPlugin=task/none
SelectType=select/cons_tres
SelectTypeParameters=CR_Core
ReturnToService=2
MpiDefault=${SLURM_MPI_DEFAULT:-pmix}
SlurmctldPidFile=/run/slurmctld.pid
SlurmdPidFile=/run/slurmd.pid
StateSaveLocation=/var/spool/slurmctld
SlurmdSpoolDir=/var/spool/slurmd
SlurmctldLogFile=/var/log/slurm/slurmctld.log
SlurmdLogFile=/var/log/slurm/slurmd.log
NodeName=${HOST} Sockets=1 CoresPerSocket=${CPUS} ThreadsPerCore=1 RealMemory=${MEM_MB} State=UNKNOWN
PartitionName=compute Nodes=${HOST} Default=YES MaxTime=INFINITE State=UP
EOF

# slurmd insists on a cgroup v2 hierarchy. There is no systemd in the container
# to create one, so tell Slurm not to ask systemd and make the parent directory
# by hand. This needs a writable /sys/fs/cgroup: run with --privileged.
printf "CgroupPlugin=cgroup/v2\nIgnoreSystemd=yes\n" | sudo tee /etc/slurm/cgroup.conf > /dev/null
if ! sudo mkdir -p /sys/fs/cgroup/system.slice 2> /dev/null; then
    echo "/sys/fs/cgroup is read-only; start the container with --privileged" >&2
    exit 1
fi

sudo mkdir -p /run/munge /var/spool/slurmctld /var/spool/slurmd /var/log/slurm
sudo chown munge:munge /run/munge
sudo chown slurm:slurm /var/spool/slurmctld /var/log/slurm
if ! sudo test -s /etc/munge/munge.key; then
    sudo dd if=/dev/urandom of=/etc/munge/munge.key bs=1024 count=1 status=none
    sudo chown munge:munge /etc/munge/munge.key
    sudo chmod 400 /etc/munge/munge.key
fi

pgrep -x munged > /dev/null    || sudo -u munge /usr/sbin/munged
pgrep -x slurmctld > /dev/null || sudo /usr/sbin/slurmctld
pgrep -x slurmd > /dev/null    || sudo /usr/sbin/slurmd

# Wait until the node reports idle
for _ in $(seq 1 30); do
    if sinfo -h -o '%T' 2> /dev/null | grep -q idle; then
        sinfo
        exit 0
    fi
    sleep 1
done

echo "Slurm did not come up; see /var/log/slurm/" >&2
sinfo >&2 || true
exit 1
