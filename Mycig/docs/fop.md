```mycig
package main

import std::fmt

frame Animal {
    abs func move()
}

frame Pet: Animal {
    field Self {
        *name: str
    }

    impl Self {
        abs init new(*name)
        abs func greet(self)
        abs func getName(self)
    }
}

frame Cat: Pet {
    field Pet {
        *name: str
    }

    sealed impl Pet {
        pub init new(*name) {
            *name: name
        }
        pub func greet(self) {
            fmt::println("Hello! I'm {}.", self.name)
        }
        pub func getname(self) {
            self.name
        }
    }
}

frame Dog: Pet {
    field Pet {
        *name: str
    }

    sealed impl Pet {
        pub init new(*name) {
            *name: name
        }
        pub func greet(self) {
            fmt::println("Hello! I'm {}.", self.name)
        }
        pub func getname(self) {
            self.name
        }
    }
}

func greet(pet: Pet) {
    pet.greet()
}

func main() {
    let *cat = Cat::new(*"Lucy")
    let *dog = Dog::new(*"Tom")
    
    greet(cat as Pet)
    greet(dog as pet)
}
```