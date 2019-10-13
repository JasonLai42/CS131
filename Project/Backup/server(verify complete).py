import asyncio
import aiohttp
import json
import sys
import time
import re

# Hardcode dictionary with key as servername and value as its connections
connections = {
    "Goloman": ["Hands", "Holiday", "Wilkes"],
    "Hands": "Wilkes",
    "Holiday": ["Welsh", "Wilkes"]
}

# My ports: 12048 (START) - 12056 (END)
ports = {
    "Goloman": 12048,
    "Hands": 12049,
    "Holiday": 12050,
    "Welsh": 12051,
    "Wilkes": 12052
}

#####################################################################
#                     Process & Verify Requests                     #
#####################################################################

def tokenize_request(command):
    # Break request up into a list of words
    return command.strip().split()

def is_number(x):
    try:
        float(x)
        return 1
    except ValueError:
        return -1

def get_geo_coord(lat_long):
    signs = []
    for i in lat_long:
        if i == '+' or i == '-':
            signs.append(i)

    if len(signs) != 2:
        return None

    coordinates = re.split("[\+\-]", lat_long)
    coordinates.pop(0)

    if len(coordinates) != 2:
        return None

    for i in coordinates:
        if is_number(i) == -1:
            return None

    final_coords = list(x+str(y) for x, y in zip(signs, coordinates))
    return final_coords

def verify_IAMAT(request):
#    format_msg = []
#    format_msg.append(request[0])
#    format_msg.append(request[1])

    coords = get_geo_coord(request[2])
    if coords is None:
        return -2
#    else:
#        format_msg.append(coords)

    if is_number(request[3]) == -1:
        return -3
#    else:
#        format_msg.append(request[3])

    return 1

def verify_WHATSAT(request):
#    format_msg = []
#    format_msg.append(request[0])
#    format_msg.append(request[1])

    if is_number(request[2]) == -1:
        return -4

    if request[2] <= 0 or request[2] > 50:
        return -4
#    else:
#        format_msg.append(request[2])

    try:
        int(request[3])
        if request[3] <= 0 or request[3] > 20:
            return -5
#        else:
#            format_msg.append(request[3])
    except ValueError:
        return -5

    return 2

def process_request(request):
    # Only take in requests of the form [command][clientID[number][number] ~ length 4
    if len(request) != 4:
        return -1

    if request[0] == "IAMAT":
        return verify_IAMAT(request)
    elif request[0] == "WHATSAT":
        return verify_WHATSAT(request)
    else:
        return -1

    return -1

#####################################################################
#                         Requests Handlers                         #
#####################################################################

def do_IAMAT():
    

def do_WHATSAT():

def handle_request():

#####################################################################
#                           Process Output                          #
#####################################################################

def handle_error(protocol_number):
    # -1 is bad request
    # -2 is bad coordinate pair (IAMAT)
    # -3 is bad timestamp (IAMAT)
    # -4 is bad radius (WHATSAT)
    # -5 is bad upper bound (WHATSAT)

def get_time_diff(sent_timestamp, received_timestamp):

def process_response():

#####################################################################
#                         Event Loop Handler                        #
#####################################################################

async def handle_echo(reader, writer):
    in_data = await reader.read(100)
    # GET TIMESTAMP OF MESSAGE RECEIVED
    message = in_data.decode()

    # Process received data
    msg_list = tokenize_request(message)
    protocol_number = process_request(msg_list)

    # Proceed with given protocol
    if protocol_number > 0:
        # PROPAGATE REQUEST

        # Process response
        time_diff = get_time_diff(msg_list[3], # RECEIVED TIMESTAMP)
        response = process_response(msg_list, time_diff)
    else:
        # If request verification sent back an error, handle it
        response = handle_error(protocol_number)

    # Send back response
    out_data = response.encode()
    writer.write(out_data)
    await writer.drain()

    writer.close()

#####################################################################
#                                Main                               #
#####################################################################

async def main():
    if len(sys.argv) != 2:
        print("Error: invalid arguments")
        sys.exit(1)
    if sys.argv[1] not in ports:
        print("Error: servername not found")
        sys.exit(1)

    # Depending on the passed servername, initialize the port
    server = await asyncio.start_server(handle_echo, '127.0.0.1', ports[sys.argv[1]])
    addr = server.sockets[0].getsockname()

    # NEED TO USE CLIENT API TO HAVE CURRENT SERVER ACT AS CLIENT AND COMMUNICATE FOR FLOODING ALGORITHM
    ''' In report talk about: Asyncio doesn't guarantee all servers are up to date at any given 
        time, so talk about the case where one server gets an IAMAT and another gets WHATSAT, but 
        the WHATSAT server didn't get the propagated data yet, then it returns invalid since it has 
        nothing. '''

    async with server:
        await server.serve_forever()

asyncio.run(main())