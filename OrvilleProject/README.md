## Orville Project

To build this project you would require [Haskell] (https://www.haskell.org/ghcup/) installed. 

Try to setup [GHCup] (https://www.haskell.org/ghcup/guide/) and download the required versions of stack , cabal 

## How To Build Project

`git clone` [GitHub Repository]


## How To Use Docker For Project

Change directory to the cloned project. \
You could see one script in `run-application.sh`.\
Required docker command [docker-compose up] is provided in that file.

Use `./run-application.sh` to execute script 


## Docker Desktop

![Containers Built On Docker Desktop](<Screenshot from 2024-08-23 16-19-10.png>)


![Images Built On Docker Desktop](<Screenshot from 2024-08-23 16-19-27.png>)


Use (http://localhost:3000/your-table-name) to work with API. \
Your table name could be different, depending upon updation made in code. \


Checkout (https://docs.docker.com/manuals) to learn more about [Docker]




## How To Read .env File

I have used `git-crypt` to perform this task. \
Use `git-crypt unlock ../git-encrypt-key-to-decrypt`

