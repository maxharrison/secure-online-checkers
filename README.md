# Secure Online Checkers

COMP3003 - Computer Science BSc

20186538 - Supervised by Graham Hutton

## User Manual / README

### Introduction
This user manual will guide you through the process of setting up and running the Checkers game server and the Python GUI client. The server can be run with an executable file, while the client is a Python script that provides a graphical interface to interact with the game.

### System Requirements
- A Linux computer to run the server (the server was compiled on Pop!_OS 22.04 LTS, so should work on Ubuntu 22.04)
- Python 3.10.6 or higher
- Required Python packages are listed in requirements.txt

### Installation
- Download the zip file of the program adn extract the contents to your desired location.
- Open a terminal or command prompt and navigate to the extracted folder.
- Run the following command to install the required Python packages: `pip install -r requirements.txt`.

### Running the Server
To start the server, run the provided executable file. Open a terminal, navigate to the server folder, and run `./secure_online_checkers_server`.

### Running the Python GUI Client
- Once the server is up and running, open another terminal or command prompt and navigate to the src folder. This can be done on the same computer that the server is running, or if on the same network the zip can be downloaded seperately.
- Then, run the following command to start the Python GUI client: `python Client.py`.


### How to Play
- Upon launching the Python GUI client, you will be asked the server's ip address (or leave it as 'localhost' if running on the same machine).
Click 'Connect' to join the game.
- You will then be asked if you want to play against the AI.
- Wait for an opponent to connect. Once connected, the game will automatically start.
- The white player moves first, and you select the route you want the piece to take by pressing the squares and the once you are ready, you click the green button to confirm. You can click the red button to reset the route.
