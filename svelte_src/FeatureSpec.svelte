<script>
    import { Alert,
             Button,
             Col,
             Collapse,
             Card,
             CardBody,
             CardFooter,
             CardHeader,
             CardSubtitle,
             CardText,
             CardTitle,
             Container,
             CustomInput,
             Input,
             ListGroup,
             ListGroupItem,
             Form,
             FormGroup,
             Row} from "sveltestrap";

    import CloseIcon from './CloseIcon.svelte';
    import FeatureSpecUserType from './FeatureSpecUserType.svelte';

    export let spec = {};
    let isOpen = false;
    let saveAlertVisibile = false;
    let failAlertVisibile = false;
    let failAlertMessage = "";

    function toggle() {
        isOpen = !isOpen;
    };

    async function save() {
        // Reset notifications
        saveAlertVisibile = false;
        failAlertVisibile = false;

        let response = await fetch('/v0/featureSpecs/', {
            method: 'POST',
            headers: {
                'content-type': 'application/json'
            },
            body: JSON.stringify(spec)
        });
        let fulfilledResponse = await response;
        if ( fulfilledResponse.ok) {
            saveAlertVisibile = true;
        } else {
            let responseObj = await fulfilledResponse.json()
            console.log(responseObj);
            failAlertVisibile = true;
            failAlertMessage = responseObj['error']['what'];
        };
    };

    function ISODateString(d){
        function pad(n){return n<10 ? '0'+n : n}
        return d.getUTCFullYear()+'-'
            + pad(d.getUTCMonth()+1)+'-'
            + pad(d.getUTCDate())+'T'
            + pad(d.getUTCHours())+':'
            + pad(d.getUTCMinutes())+':'
            + pad(d.getUTCSeconds())+'Z'
    };

    function addRolloutSpec() {
        let now = new Date();
        let defaultEnd = new Date(now.valueOf());
        defaultEnd.setDate(defaultEnd.getDate() + 2);

        spec.rollout_start = ISODateString(now);
        spec.rollout_end = ISODateString(defaultEnd);
    };

    function removeRolloutSpec() {
        delete spec.rollout_end;
        delete spec.rollout_start;
        spec = spec;
    };

    function addNewUserSpec() {
        let newUserSpec = {
            "property": "user property",
            "comparator": "=",
            "value": "user value"
        };
        spec.user = [...spec.user, newUserSpec];
    };

    function removeUserSpecCallback(userSpec) {
        return function () {
            spec.user = spec.user.filter(i => i !== userSpec);
        };
    };

</script>

    <Col xs="auto" align="center">
    <Card>
        <CardHeader on:click={toggle} >
            <CardTitle>{spec.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>

            <Row>
                <Col>
                    <Alert color="success" isOpen={saveAlertVisibile} toggle={() => (saveAlertVisibile = false)}>
                        Feature Flag Saved!
                    </Alert>
                    <Alert color="danger" isOpen={failAlertVisibile} toggle={() => (failAlertVisibile = false)}>
                        Feature Flag NOT Saved!
                        {failAlertMessage}
                    </Alert>
                </Col>

            </Row>

            <Row>
                <Col xs=2 >Always True</Col>
                <Col xs=4 ><CustomInput id="{spec.name}-boolean"
                                        name="boolean"
                                        type="switch"
                                        bind:checked={spec.boolean} />
                </Col>
                <Col xs=2></Col>
            </Row>
            <hr />

            {#if !spec.boolean && spec.rollout_start }
            <Row no-gutters>
                <Col>
                    <Row>
                        <Col xs=2>Rollout Start: </Col>
                        <Col xs=6>
                            <Input
                             type="datetime"
                             name="rollout_start"
                             id="exampleDatetime"
                             placeholder="{spec.rollout_start}"
                             bind:value="{spec.rollout_start}" />
                        </Col>
                        <Col xs=2 />
                    </Row>
                    <Row>
                        <Col xs=2>Rollout End:</Col>
                        <Col xs=6>
                            <Input
                            type="datetime"
                            name="rollout_end"
                            id="exampleDatetime"
                            placeholder="{spec.rollout_end}"
                            bind:value="{spec.rollout_end}" />
                        </Col>
                        <Col xs=2 />
                    </Row>
                </Col>
                <Col xs=2>
                    <Button color=dark outline on:click="{removeRolloutSpec}"><CloseIcon /></Button>
                </Col>

            </Row>
            <hr />
            {/if}

            {#if !spec.boolean }
            {#each spec.user as userSpec }
            <Row>
                <Col>
                    <Row>
                        <Col xs=2>
                            User
                        </Col>
                        <Col xs=8>
                            <FeatureSpecUserType userSpec={userSpec} />
                        </Col>
                    </Row>
                </Col>
                <Col xs=2>
                    <Button color=dark outline on:click="{removeUserSpecCallback(userSpec)}"><CloseIcon /></Button>
                </Col>
            </Row>
            <hr />
            {/each}
            {/if}


            <Row>
                <Col></Col>
                <Col xs=3>
                {#if !spec.boolean && !spec.rollout_start }
                    <Button on:click="{addRolloutSpec}">Add rollout spec</Button>
                {/if}
                </Col>

                <Col xs=3>
                    {#if !spec.boolean }
                    <Button on:click="{addNewUserSpec}">Add user spec</Button>
                    {/if}
                </Col>

                <Col xs=2>
                    <Button type="submit" on:click={save} >Save</Button>
                </Col>
            </Row>

        </CardBody>
        </Collapse>

    </Card>
    </Col>

<style>
</style>
