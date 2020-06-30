<script>
    import {

             Col,
             Collapse,
             Dropdown,
             DropdownToggle,
             DropdownItem,
             Input,
             ListGroup,
             ListGroupItem,
             Row} from "sveltestrap";

    export let userSpec = [];
    let isOpen = false;

</script>

<main>
  <ListGroup>
  <ListGroupItem>
      {userSpec[0]}
      {userSpec[1]}
      {userSpec[2]}
  </ListGroupItem>
  </ListGroup>

</main>

<style>
</style>
