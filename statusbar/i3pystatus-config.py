from i3pystatus import Status

status = Status()

status.register("clock",
                format="%a %d %b %H:%M")

# status.register("load")

status.register("temp",
                format="{temp:.0f}°C",)

status.register("battery",
                format="{status} {consumption:.1f}W {percentage:.0f}%",
                alert=True,
                alert_percentage=5,
                status={
                    "DIS": "↓",
                    "CHR": "↑",
                    "FULL": "=",
                })

status.register("mpd",
                format="{status}",
                status={
                    "pause": "=",
                    "play": "▶",
                    "stop": "◾",
                },)

status.run()
