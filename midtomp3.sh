# Thanks to:
#   Source - https://stackoverflow.com/a/16296003
#   Posted by phreaknerd
#   Retrieved 2026-08-11, License - CC BY-SA 3.0

set -e

timidity $1 -Ow -o - | ffmpeg -i - -acodec libmp3lame -ab 64k $2
