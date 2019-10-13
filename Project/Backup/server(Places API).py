import asyncio
import aiohttp
import json
import sys
import time
import re

# Google Places API key
API_KEY = "AIzaSyAe6BULCewKb1OtTSSThVAhZ_TgRERb2Tw"

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

# Save client information
clients = {}

#####################################################################
#                     Process & Verify Requests                     #
#####################################################################

# Break request up into a list of words
def tokenize_request(command):
    return command.strip().split()

# Test if something is a number
def is_number(x):
    try:
        float(x)
        return 1
    except ValueError:
        return -1

# Separate and verify latitude and longitude
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

# Verify IAMAT operands
def verify_IAMAT(request):
#    format_msg = []
#    format_msg.append(request[0])
#    format_msg.append(request[1])

    coords = get_geo_coord(request[2])
    if coords is None:
        return -1
#    else:
#        format_msg.append(coords)

    if is_number(request[3]) == -1:
        return -1
#    else:
#        format_msg.append(request[3])

    return 1

# Verify WHATSAT operands
def verify_WHATSAT(request):
#    format_msg = []
#    format_msg.append(request[0])
#    format_msg.append(request[1])

    if is_number(request[2]) == -1:
        return -1

    if request[2] <= 0 or request[2] > 50:
        return -1
#    else:
#        format_msg.append(request[2])

    try:
        int(request[3])
        if request[3] <= 0 or request[3] > 20:
            return -1
#        else:
#            format_msg.append(request[3])
    except ValueError:
        return -1

    return 2

# Verify a request
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

# Returns difference between received and sent time; else for exhaustion
def get_time_diff(sent_timestamp, received_timestamp):
    time_diff = float(received_timestamp) - float(sent_timestamp)
    if time_diff > 0:
        final_diff = "+" + str(time_diff)
    else:
        final_diff = None
    
    return final_diff

# Handle IAMAT request
def do_IAMAT(msg_list, received_timestamp):
    client_info = []
    client_info.append(sys.argv[1])

    time_diff = get_time_diff(msg_list[3], received_timestamp)
    if time_diff is not None:
        client_info.append(msg_list[3])
        client_info.append(received_timestamp)
    else:
        return ("? {0}".format(' '.join(msg_list)))

    client_info.extend(msg_list[1:])
    clients[msg_list[1]] = client_info

    # NEED TO FLOOD OTHER SERVERS

    response = ("AT {0} {1} {2}\n".format(sys.argv[1], time_diff, ' '.join(msg_list[1:])))
    return response

# Handle WHATSAT request
def do_WHATSAT(msg_list):
    if msg_list[1] not in clients:
        return ("? {0}".format(' '.join(msg_list)))
    else:
        # API usage: https://developers.google.com/places/web-service/search
        client_info = clients[msg_list[1]]
        lat_long = get_geo_coord(client_info[3])
        radius = float(msg_list[2]) * 1000.0
        HTTP_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1},{2}&radius={3}&".format(API_KEY, lat_long[0], lat_long[1], radius)

        # aiohttp usage: https://docs.aiohttp.org/en/stable/client_quickstart.html#passing-parameters-in-urls
        async with aiohttp.ClientSession() as session:
            async with session.get(HTTP_URL) as resp:
                json_out = await resp.json()
                

        time_diff = get_time_diff(client_info[1], client_info[2])
        response = ("AT {0} {1} {2}\n".format(client_info[0], time_diff, ' '.join(client_info[3:])))
    
        return response
    
# Handle either IAMAT or WHATSAT
def handle_request(protocol_number, msg_list, received_timestamp):
    if(protocol_number == 1):
        return do_IAMAT(msg_list, received_timestamp)
    else:
        return do_WHATSAT(msg_list)

#####################################################################
#                         Event Loop Handler                        #
#####################################################################

async def handle_echo(reader, writer):
    in_data = await reader.read(100)
    timestamp = time.time()
    message = in_data.decode()

    # Process and verify received data
    msg_list = tokenize_request(message)
    protocol_number = process_request(msg_list)

    # Proceed with given protocol
    response = ""
    if protocol_number > 0:
        # Handle either IAMAT or WHATSAT
        response = handle_request(protocol_number, msg_list, timestamp)
    else:
        # If request verification sent back an error, handle it
        response = ("? {0}".format(message))

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