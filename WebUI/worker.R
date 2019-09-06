# Set up a worker for evaluation of InterventionEvaluatR
setupWorker = function() {
  if(getOption("ie.worker.local", TRUE)) {
    setupLocalWorker()
  } else {
    setupRemoteWorker()
  }
}

setupLocalWorker = function() {
  list(local=TRUE)
}

setupRemoteWorker = function() {
  # TODO generate ephemeral SSH key
  
  machineName = sprintf("iew-%s", UUIDgenerate())
  
  # First provision a DigitalOcean droplet
  analysisStatusDetail("Provisioning DO droplet")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "create",
      "--driver", "digitalocean",
      "--digitalocean-access-token", getOption("ie.digitalocean.access.token"),
      "--digitalocean-size", getOption("ie.worker.digitalocean-droplet-size", "s-2vcpu-4gb"),
      "--digitalocean-userdata", "worker/cloud-config.yml",
      machineName
    )
  )
  
  workerConfig = check.output(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "config",
      machineName
    )
  )
  
  workerIp = check.output(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ip",
      machineName
    )
  ) %>% trimws()
  
  # Copy the worker image
  analysisStatusDetail("Copying worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "scp",
      "worker/image.tar.xz", sprintf("%s:/tmp/worker-image.tar.xz", machineName)
    )
  )
  
  # Load the worker image into docker
  analysisStatusDetail("Unarchiving worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ssh",
      machineName, "/usr/bin/unxz", "/tmp/worker-image.tar.xz"
    )
  )
  
  analysisStatusDetail("Loading worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ssh",
      machineName, sprintf("%s/docker", getOption("ie.worker.docker.bindir")), "load", "--input", "/tmp/worker-image.tar"
    )
  )
  
  # Make a single-worker cluster 
  workerCluster = makeClusterPSOCK(
    workers=workerIp,
    rshopts=c("-i", "worker/id_rsa", "-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null"),
    user="evaluatr",
    rscript="/usr/local/bin/Rscript-docker"
  )
  
  list(local=FALSE, cluster=workerCluster, machineName=machineName)
}

# Generate a future evaluation plan for our worker
workerPlan = function(worker) {
  if(worker$local) {
    localWorkerPlan(worker)
  } else {
    remoteWorkerPlan(worker)
  }
}

localWorkerPlan = function(worker) {
  plan()
}

remoteWorkerPlan = function(worker) {
  plan(cluster, workers=worker$cluster)
}

# Shut down the worker
dismissWorker = function(worker) {
  if(worker$local) {
    dismissLocalWorker(worker)
  } else {
    dismissRemoteWorker(worker)
  }
}

dismissLocalWorker = function(worker) {
}

dismissRemoteWorker = function(worker) {
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "rm", "--force",
      worker$machineName
    )
  )
}

check.call = function(args) {
  status = system2(args[1], args[2:length(args)], stdout="", stderr="")
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return()
  } else {
    stop(sprintf("status = %d", status))
  }
}

check.output = function(args) {
  res = system2(args[1], args[2:length(args)], stdout=TRUE, stderr="")
  status = attr(res, "status", exact=TRUE)
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return(res)
  } else {
    stop(sprintf("status = %d", status))
  }
}

