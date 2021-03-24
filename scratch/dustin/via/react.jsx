
function UserGreeting(props) {
    return <h1>Welcome back!</h1>;
}

function GuestGreeting(props) {
    return <h1>Please sign up.</h1>;
}

    let Greeting = props => {
        <Wrapper>
            <div><PromptButtonLogo isLoggedIn={props.isLoggedIn}/></div>
            {isLoggedIn ? <UserGreeting props={props}/> : <GuestGreeting props={props}/>}
        </Wrapper>;}

function Greeting(props) {
    return ()}

ReactDOM.render(
    // Try changing to isLoggedIn={true}:
    <Greeting isLoggedIn={false} />,
    document.getElementById('root')
);

