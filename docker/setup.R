## Making a Synapse Config file
# This file lives in your home directory, and will provide your credentials to your Synapse R, Python and command-line clients. If you are creating this in the hackathon docker container, please remember that files created in the running containers are transient - if you stop the container, you'll have to recreate this configuration file. 
# 
# Also, after editing the following snippet, do not save the file and push to Github! If you accidentally push your synapse credentials to github, you'll need to run a tool like BFG Repo Cleaner to scrub them from your repository.
# 
# All this being said, to create a synapseConfig file, you can simply edit "username" to your Synapse username and "password" in the following snippet, uncomment the final line (`writeLines`) and then run it to create a .synapseConfig file in your home directory.

username <- "username"
password <- "password"

creds <- paste0("[authentication]\n",
                "username = ", username, "\n",
                "password = ", password)

#uncomment this line
writeLines(creds, "~/.synapseConfig")

## Testing your configuration file
# Let's check that this worked by loading the `synapser` client and logging in. Uncomment 
# After running `synLogin()`, you should see a message like `Welcome, FirstName LastName!``

library(synapser)
synLogin()

##and config git profile
git_email <- "a@b.com"
git_name <- "Doug Funnie"

system2("git", args = sprintf("config --global user.email '%s'", git_email))
system2("git", args = sprintf("config --global user.name '%s'", git_name))
