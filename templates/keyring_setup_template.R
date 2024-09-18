# install.packages("keyring")

# Set key for a service named "cmpdb" for the CMP database
# Will open up a prompt to enter the password to associate with this username
keyring::key_set(service = "cmpdb",
                 username = "USERNAME")

# Retrieves the key for a service named "cmpdb" for the CMP database
password <- keyring::key_get(service = "cmpdb",
                             username = "USERNAME")
