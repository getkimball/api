<script>
    import { onMount } from "svelte";
    import { Button,
             Col,
             Container,
             FormGroup,
             Input,
             Row } from "sveltestrap";


    export let item = {};

</script>

{ item.name } - { item.count }

