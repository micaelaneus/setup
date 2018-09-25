#!/bin/bash

function countdown() {
    date_end=$((`gdate +%s` + $1));
    date_next=$((`gdate +%s` + $2));
    play -q "${HOME}/Dropbox/org_mode/quiet_time_sound_short.mp3" &
    while [ "${date_end}" -ge `gdate +%s` ]; do
        echo -ne "$(gdate -u --date @$((${date_end} - `gdate +%s`)) +%H:%M:%S)\r";
        while [ "${date_next}" -le `gdate +%s` ] && [ "${date_end}" -ge `gdate +%s` ]; do
            play -q "${HOME}/Dropbox/org_mode/quiet_time_sound_short.mp3" &
            date_next=$((${date_next} + $1));
        done
        sleep 0.25
    done
    play -q "${HOME}/Dropbox/org_mode/quiet_time_sound_long.wav" &
}

countdown 1800 120
