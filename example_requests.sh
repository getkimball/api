set -e

export USER=$(echo '{"user_id": 41}' | base64)
export CTX=$(echo '{"feature": "features_post"}' | base64)

#curl -vf "http://127.0.0.1:8080/v0/features?user_obj=$USER&context_obj=$CTX"

HOST=http://127.0.0.1:8080

curl -f -H "content-type: application/json" \
        -d "{\"goal_name\": \"goal\"}" \
     "${HOST}/v0/goals"

send_event () {
    NAMESPACE=$1
    EVENT_NAME=$2
    USER_ID=$3

    curl -f -H "content-type: application/json" \
            -d "{\"namespace\": \"${NAMESPACE}\", \"event_name\": \"${EVENT_NAME}\", \"user_id\": \"${USER_ID}\"}" \
            "${HOST}/v0/analytics"
}

send_event_value () {
    NAMESPACE=$1
    EVENT_NAME=$2
    USER_ID=$3
    VALUE=$4

    curl -f -H "content-type: application/json" \
            -d "{\"namespace\": \"${NAMESPACE}\", \"event_name\": \"${EVENT_NAME}\", \"user_id\": \"${USER_ID}\", \"value\": ${VALUE} }" \
            "${HOST}/v0/analytics"
}

send_goal_event () {
    NAMESPACE=$1
    EVENT_NAME=$2
    USER_ID=$3

    curl -f -H "content-type: application/json" \
            -d "{\"namespace\": \"${NAMESPACE}\", \"event_name\": \"${EVENT_NAME}\", \"user_id\": \"${USER_ID}\", \"ensure_goal\": true }" \
            "${HOST}/v0/analytics"
}


END=200
RAND_MOD=50

# Generic example

generic_example () {
    for i in $(seq 1 $END); do
        R=$(( ( RANDOM % $RAND_MOD )  + 1 ))
        echo $i - $R of $RAND_MOD
        send_event default analytics_post "${i}"
        send_event_value default value "${i}" 1

        curl -f -H "content-type: application/json" \
                -d "{ \"events\": [{\"event_name\": \"multi_submit_1\", \"user_id\": \"$i\"},{\"event_name\": \"multi_submit_2\", \"user_id\": \"$i\"}]}" \
                "${HOST}/v0/analytics"

        send_event default "step ${R}" ${i}
        send_goal_event default "analytics_post_goal" "${i}"
        send_event default "goal" "${i}"

        if (( RANDOM % 2 )); then
            send_goal_event default "chance_goal" "${i}"
        fi

    done
}

saas_example() {

    for i in $(seq 1 $END); do
        P=$(( ( RANDOM % 100 )  + 1 ))
        echo "${i}"

        send_event saas_example view_any_page "${i}"

        if [[ $P -le 80 ]]; then
            send_goal_event saas_example "signup" "${i}"
        fi

        if [[ $P -le 75 ]]; then
            send_goal_event saas_example "login" "${i}"
        fi

        if [[ $P -le 60 ]]; then
            send_goal_event saas_example "feature_a" "${i}"
        fi

        if [[ $P -ge 50 ]]; then
            send_goal_event saas_example "feature_b" "${i}"
        fi

    done
}


non_user_example() {

    for i in $(seq 1 $END); do
        P=$(( ( RANDOM % 100 )  + 1 ))
        CUSTOMER=$(( ( RANDOM % 4 )  + 1 ))
        OTHER=$(( ( RANDOM % 3 )  + 1 ))
        echo "${i}" "${CUSTOMER}" "${OTHER}"

        send_event invoice_example created "${i}"
        send_event invoice_example "object-type: invoice" "${i}"
        send_event invoice_example "customer-id: ${CUSTOMER}" "${i}"

        if [[ $OTHER = 1 ]]; then
            send_event invoice_example "status: created" "${i}"
        fi
        if [[ $OTHER = 2 ]]; then
            send_event invoice_example "status: created" "${i}"
            send_event invoice_example "status: sent" "${i}"
        fi
        if [[ $OTHER = 3 ]]; then
            send_event invoice_example "status: created" "${i}"
            send_event invoice_example "status: sent" "${i}"
            send_goal_event invoice_example "status: paid" "${i}"
        fi

    done
}


generic_example
saas_example
non_user_example
