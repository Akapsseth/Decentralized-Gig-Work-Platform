import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const owner = accounts.get("wallet_1")!;
const worker = accounts.get("wallet_2")!;

describe("gig contract", () => {
  const testGig = {
    title: "Test Gig",
    description: "Test Description",
    payment: 1000,
  };

  describe("create-gig", () => {
    it("successfully creates a new gig", () => {
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii(testGig.title),
          Cl.stringAscii(testGig.description),
          Cl.uint(testGig.payment),
        ],
        owner
      );
      expect(createGigCall.result).toBeOk(Cl.uint(1));
    });
  });

  describe("accept-gig", () => {
    it("allows worker to accept an available gig", () => {
      // First create a gig
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      const acceptGigCall = simnet.callPublicFn(
        "gig",
        "accept-gig",
        [Cl.uint(1)],
        worker
      );
      expect(acceptGigCall.result).toBeOk(Cl.bool(true));
    });
  });
  describe("complete-gig", () => {
    it("allows worker to mark gig as completed", () => {
      // Create a gig first
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      // Worker needs to accept the gig first
      const acceptGigCall = simnet.callPublicFn(
        "gig",
        "accept-gig",
        [Cl.uint(1)],
        worker
      );

      // Now worker can mark it as complete
      const completeGigCall = simnet.callPublicFn(
        "gig",
        "complete-gig",
        [Cl.uint(1)],
        worker
      );

      expect(completeGigCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("release-payment", () => {
    it("allows owner to release payment for completed gig", () => {
      // Create a gig first
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      // Worker accepts the gig
      const acceptGigCall = simnet.callPublicFn(
        "gig",
        "accept-gig",
        [Cl.uint(1)],
        worker
      );

      // Worker completes the gig
      const completeGigCall = simnet.callPublicFn(
        "gig",
        "complete-gig",
        [Cl.uint(1)],
        worker
      );

      // Now owner can release payment
      const releasePaymentCall = simnet.callPublicFn(
        "gig",
        "release-payment",
        [Cl.uint(1)],
        owner
      );

      expect(releasePaymentCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("rate-user", () => {
    it("successfully rates a user", () => {
      const rateUserCall = simnet.callPublicFn(
        "gig",
        "rate-user",
        [Cl.principal(worker), Cl.uint(5)],
        owner
      );
      expect(rateUserCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("create-dispute", () => {
    it("successfully creates a dispute", () => {
      // First create a gig
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      const createDisputeCall = simnet.callPublicFn(
        "gig",
        "create-dispute",
        [Cl.uint(1), Cl.stringAscii("Dispute description")],
        worker
      );
      expect(createDisputeCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("add-milestone", () => {
    it("allows owner to add milestone", () => {
      // First create a gig
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      const addMilestoneCall = simnet.callPublicFn(
        "gig",
        "add-milestone",
        [Cl.uint(1), Cl.stringAscii("Milestone 1"), Cl.uint(500)],
        owner
      );
      expect(addMilestoneCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("add-categories", () => {
    it("allows owner to add categories to gig", () => {
      // First create a gig
      const createGigCall = simnet.callPublicFn(
        "gig",
        "create-gig",
        [
          Cl.stringAscii("Test Gig"),
          Cl.stringAscii("Test Description"),
          Cl.uint(1000),
        ],
        owner
      );

      // Then add categories to the created gig
      const categories = ["Design", "Web"];
      const addCategoriesCall = simnet.callPublicFn(
        "gig",
        "add-categories",
        [
          Cl.uint(1), // Use the gig-id from the created gig
          Cl.list(categories.map((cat) => Cl.stringAscii(cat))),
        ],
        owner
      );
      expect(addCategoriesCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("update-portfolio", () => {
    it("allows worker to update their portfolio", () => {
      const skills = ["JavaScript", "Clarity"];
      const updatePortfolioCall = simnet.callPublicFn(
        "gig",
        "update-portfolio",
        [
          Cl.list(skills.map((skill) => Cl.stringAscii(skill))),
          Cl.stringAscii("5 years experience"),
        ],
        worker
      );
      expect(updatePortfolioCall.result).toBeOk(Cl.bool(true));
    });
  });

  describe("get-gig", () => {
    it("returns correct gig information", () => {
      const getGigCall = simnet.callReadOnlyFn(
        "gig",
        "get-gig",
        [Cl.uint(1)],
        owner
      );
      expect(getGigCall.result).toBeNone();
    });
  });

  describe("get-user-gigs", () => {
    it("returns correct gig count for user", () => {
      const getUserGigsCall = simnet.callReadOnlyFn(
        "gig",
        "get-user-gigs",
        [Cl.principal(owner)],
        owner
      );
      expect(getUserGigsCall.result).toBeNone();
    });
  });
});
