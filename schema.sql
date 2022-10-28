--
-- PostgreSQL database dump
--

-- Dumped from database version 14.5 (Debian 14.5-2.pgdg110+2)
-- Dumped by pg_dump version 14.4

-- Started on 2022-10-27 21:28:35

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_table_access_method = heap;

--
-- TOC entry 210 (class 1259 OID 24577)
-- Name: comments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comments (
    id uuid NOT NULL,
    title text NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    post_id uuid NOT NULL
);


--
-- TOC entry 209 (class 1259 OID 16385)
-- Name: posts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posts (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title text NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- TOC entry 3174 (class 2606 OID 24583)
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- TOC entry 3172 (class 2606 OID 16392)
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- TOC entry 3175 (class 1259 OID 24589)
-- Name: fki_post_id_fk; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX fki_post_id_fk ON public.comments USING btree (post_id);


--
-- TOC entry 3176 (class 2606 OID 24584)
-- Name: comments post_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT post_id_fk FOREIGN KEY (post_id) REFERENCES public.posts(id) NOT VALID;


-- Completed on 2022-10-27 21:28:35

--
-- PostgreSQL database dump complete
--

