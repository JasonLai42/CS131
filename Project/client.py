import asyncio
import sys

async def tcp_echo_client(message):
    reader, writer = await asyncio.open_connection('127.0.0.1', 12048)
    writer.write(message.encode())
    writer.write_eof()

    data = await reader.read(-1)
    print(data.decode())

    writer.close()
    await writer.wait_closed()

asyncio.run(tcp_echo_client(sys.argv[1]))