
return {
   -- systemd units
   termite               = "termite",
   ranger                = "termite -t ranger -e ranger",
   syslog                = "termite -t syslog -e \"sudo journalctl -n10 -f\"",
   octave                = "termite -t octave -e octave",
   sage                  = "termite -t sage   -e sage",
   docs                  = "dwb -p docs"
}
