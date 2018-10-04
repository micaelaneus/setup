#!/bin/bash

pmset noidle &
noidle_pid="${!}"
function finish_noidle() {
    kill "${noidle_pid}"
}
trap finish_noidle EXIT

function countdown() {
    date_end=$((`gdate +%s` + $1));
    date_next=$((`gdate +%s` + $2));
    play -q "/Volumes/GoogleDrive/My Drive/org_mode/fa_quiet_time_sound_short.mp3" &
    while [ "${date_end}" -ge `gdate +%s` ]; do
        echo -ne "$(gdate -u --date @$((${date_end} - `gdate +%s`)) +%H:%M:%S)\r";
        while [ "${date_next}" -le `gdate +%s` ] && [ "${date_end}" -ge `gdate +%s` ]; do
            play -q "/Volumes/GoogleDrive/My Drive/org_mode/fa_quiet_time_sound_short.mp3" &
            date_next=$((${date_next} + $2));
        done
        sleep 0.25
    done
    play -q "/Volumes/GoogleDrive/My Drive/org_mode/fa_quiet_time_sound_long.wav" &
}

countdown 1800 120
