# InterventionEvaluatR web app

## Overall architecture

The InterventionEvaluatR web app consists of two main pieces (InterventionEvaluatR web UI and InterventionEvaluatR worker), and two auxiliary pieces (Shiny proxy and nginx proxy). All these pieces are bound together using Docker and Docker Compose.

### InterventionEvaluatR web UI

This is an R Shiny app that is responsible for presenting the web UI that you probably think of as "the app" — the setup screen, the analysis progress information, and the analysis results. It performs some calculations, but the bulk of the analysis is performed by the worker, not the web UI.

If you want to change how the web app looks, you want to make changes to this piece. Start in the WebUI folder and read readme.md in there.

The web app does not automatically pick up changes to the base InterventionEvaluatR package. See WebUI/readme.md if you need to update the web app to a newer InterventionEvaluatR.

### InterventionEvaluarT worker

When running in the cloud, the web app performs the bulk of the calculations on an R cluster provisioned specifically for that purpose. This cluster consists of one virtual computer (aka InterventionEvaluatR worker) per analysis being run; each worker has multiple virtual CPUs. Configuration of each worker (including which operating system it runs, which version of R it runs, and which packages it has installed) is done through Docker. It is unlikely you need to change this, as the worker is configured to automatically install the exact same version of InterventionEvaluatR as the web UI uses, but if you need to make changes, and you are familiar with Docker, look in the `worker` folder. 

### Shiny proxy

Shiny Server is limited in that it cannot perform computation for more than one user at a time. As a result, compute-intensive Shiny applications require either Shiny Server Pro (which costs thousands of dollars annually), or they have to use a 3rd party add-on such as [Shiny Proxy](https://www.shinyproxy.io). Shiny Proxy is what InterventionEvaluatR web app uses; see the `shinyproxy` folder for details of the setup. This folder also contains the files for the “Loading InterventionEvaluatR” interstitial screen.  

### nginx proxy

This is a part of the kludgy workaround needed to make progress reporting work in InterventionEvaluatR. In short, trying to do progress reporting using the normal Shiny methods of sending a custom JavaScript message to the UI results in a hard crash of R, so instead the web UI writes the progress to a JSON file, and the client-side JavaScript periodically loads this file. Since Shiny can't serve even a simple JSON file while it's occupied with a calculation, nginx and [nginx-proxy](https://github.com/jwilder/nginx-proxy) are used to serve the progress file directly to the web browser. This all lives in the `nginx-proxy` folder.

## Request handling

An inbound HTTP request is handled as follows:

1. It is received by nginx proxy
2. If, based on the URL of the request, it is a request for a progress JSON file, it is served directly by nginx from a volume mounted in the nginx container.
3. Otherwise, the request is passed through to shinyproxy
4. If, based on the cookies in the request, the request is not a part of an existing R shiny session, shinyproxy starts a new instance of the web UI for a new shiny session
5. Otherwise, the request is passed to an existing instead of the web UI
6. Once an instance of the web UI is started, it handles all requests for a single shiny session (except for progres JSON requests)
7. When the user initiates an analysis, the web UI provisions a new Digital Ocean droplet (using docker-machine) and sets it up as a one-machine R cluster with InterventionEvaluatR installed
8. Analysis is run on this DO droplet, after which the droplet is deprovisioned

