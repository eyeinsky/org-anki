#!/usr/bin/env bash


# POST request with json body to AnkiConnect
req() {
    local ACTION="$1"
    local PARAMS="$2"
    local JSON="{\"action\": \"$ACTION\", \"version\": 6, \"params\": $PARAMS }"
    [[ "$3" != "" ]] && echo json $JSON
    curl \
        --header "Content-Type: application/json" \
        --request POST \
        --data "$JSON" \
        127.0.0.1:8765
    echo
}

# doesn't work
test() {
    req findNotes '{"query": "field1:Heading 4"}'
}

test
